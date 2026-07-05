-- | This module provides functions for working with the database (i.e. the set
-- of JSON files representing the packages which have been uploaded to
-- Pursuit).
module Handler.Database
  ( getAllPackageNames
  , createSearchIndexFromDatabase
  , getLatestPackages
  , lookupPackage
  , availableVersionsFor
  , getLatestVersionFor
  , insertPackage
  , SomethingMissing(..)
  ) where

import Import
import Language.PureScript.CoreFn.FromJSON (parseVersion')
import qualified Data.Aeson as A
import qualified Data.NonNull as NN
import qualified Data.Text as T
import Data.Version (Version, showVersion)
import System.Directory
  (getDirectoryContents, getModificationTime, doesDirectoryExist, getFileSize)

import Web.Bower.PackageMeta (PackageName, mkPackageName, runPackageName)
import qualified Language.PureScript.Docs as D

import Handler.Utils
import Handler.Caching (clearCache)
import SearchIndex (SearchIndex, PackageEntries, packageEntries, buildSearchIndex)

getAllPackageNames :: Handler [PackageName]
getAllPackageNames = do
  dir <- getDataDir
  contents <- liftIO $ getDirectoryContents (dir ++ "/verified/")
  return . sort . rights $ map (mkPackageName . pack) contents

getLatestPackages :: Handler [(PackageName, Version)]
getLatestPackages = do
    pkgNames <- getAllPackageNames
    pkgNamesAndTimestamps <- traverse withTimestamp pkgNames
    let latest = (map fst . take 10 . sortBy (comparing (Down . snd))) pkgNamesAndTimestamps
    catMaybes <$> traverse withVersion latest
  where
    withTimestamp :: PackageName -> Handler (PackageName, UTCTime)
    withTimestamp name = map (name,) (getPackageModificationTime name)

    withVersion :: PackageName -> Handler (Maybe (PackageName, Version))
    withVersion name = (map . map) (name,) (getLatestVersionFor name)

-- | Build the search index from the latest version of each package in the
-- database. Packages are read and decoded one at a time, and only the
-- (comparatively small) index entries for each package are retained: decoding
-- every package up front requires several gigabytes of memory, which is more
-- than the server has.
createSearchIndexFromDatabase :: Handler SearchIndex
createSearchIndexFromDatabase = do
  pkgNames <- getAllPackageNames
  entries <- catMaybes <$> traverse entriesFor pkgNames
  return (buildSearchIndex entries)
  where
  -- A package which cannot be read or decoded is skipped, rather than
  -- aborting the entire index build; in particular, decodePackageFile
  -- responds with a 500 (thrown as an exception) on invalid JSON.
  entriesFor :: PackageName -> Handler (Maybe PackageEntries)
  entriesFor name = do
    result <- tryAny $ do
      mversion <- getLatestVersionFor name
      case mversion of
        Nothing -> return Nothing
        Just version -> do
          mpkg <- hush <$> lookupPackageWithPolicy WaitWhenBusy name version
          case mpkg of
            Nothing -> return Nothing
            Just pkg -> do
              -- Force the entries fully before moving on to the next package,
              -- so that the decoded package can be garbage collected.
              let pkgEntries = packageEntries pkg
              pkgEntries `deepseq` return (Just pkgEntries)
    case result of
      Right r -> return r
      Left err -> do
        $logError ("Skipping " <> runPackageName name <>
                   " while building the search index: " <> tshow err)
        return Nothing

data SomethingMissing
  = NoSuchPackage
  | NoSuchPackageVersion
  deriving (Show, Eq, Ord)

lookupPackage :: PackageName -> Version -> Handler (Either SomethingMissing D.VerifiedPackage)
lookupPackage = lookupPackageWithPolicy FailWhenBusy

data LargeDecodePolicy = FailWhenBusy | WaitWhenBusy

lookupPackageWithPolicy :: LargeDecodePolicy -> PackageName -> Version -> Handler (Either SomethingMissing D.VerifiedPackage)
lookupPackageWithPolicy policy pkgName version = do
  file <- packageVersionFileFor pkgName version
  msize <- liftIO (catchDoesNotExist (getFileSize file))
  mpkg <- case msize of
    Nothing -> return Nothing
    Just size
      | size >= largeDecodeThreshold ->
          withLargeDecodeLock policy (decodeFrom file)
      | otherwise ->
          decodeFrom file
  case mpkg of
    Just pkg -> return (Right pkg)
    Nothing -> do
      -- Work out whether there's no such package or just no such version
      dir <- packageDirFor pkgName
      dirExists <- liftIO $ doesDirectoryExist dir
      return $ Left $ if dirExists then NoSuchPackageVersion else NoSuchPackage
  where
  -- The file is read inside the lock (rather than before queueing for it) so
  -- that threads waiting their turn do not each pin a copy of a large file's
  -- bytes. The decoded package is forced before the lock is released; the
  -- decode is eager in practice, but we make certain the allocation spike
  -- stays inside the lock.
  decodeFrom :: String -> Handler (Maybe D.VerifiedPackage)
  decodeFrom file = do
    mcontents <- liftIO (readFileMay file)
    case mcontents of
      Nothing -> return Nothing
      Just contents -> do
        pkg <- decodeVerifiedPackageFile file contents
        pkg `deepseq` return (Just pkg)

-- | Decoding a package's docs JSON transiently needs tens of times the file's
-- size in memory, and a few generated packages (react-icons, elmish-html,
-- deku, ...) have files of 10MB or more, so a handful of concurrent requests
-- for their (rarely cached, because there are so many of them) documentation
-- pages can exhaust the heap. Decodes of large files therefore run one at a
-- time; packages below the threshold - nearly all of them - are unaffected.
--
-- The queue for the lock is bounded: once 'maxLargeDecodeWaiters' threads
-- hold or await the lock, further requests fail fast with a 503 rather than
-- accumulating without limit while clients time out. The hourly search index
-- regeneration ('WaitWhenBusy') is exempt from the bound - it must not
-- silently omit a package from the index just because the server is busy, and
-- as a single sequential thread it adds at most one waiter.
withLargeDecodeLock :: LargeDecodePolicy -> Handler a -> Handler a
withLargeDecodeLock policy action = do
  app <- getYesod
  let counter = appLargeDecodeWaiters app
  admitted <- case policy of
    WaitWhenBusy -> do
      atomically (modifyTVar' counter (+ 1))
      return True
    FailWhenBusy -> atomically $ do
      n <- readTVar counter
      if n >= maxLargeDecodeWaiters
        then return False
        else do
          writeTVar counter (n + 1)
          return True
  if admitted
    then
      withMVar (appLargeDecodeLock app) (const action)
        `finally` atomically (modifyTVar' counter (subtract 1))
    else
      sendResponseStatus serviceUnavailable503
        ("Too many concurrent requests for large packages; try again shortly." :: Text)
  where
  maxLargeDecodeWaiters = 16 :: Int

largeDecodeThreshold :: Integer
largeDecodeThreshold = 5 * 1024 * 1024

availableVersionsFor :: PackageName -> Handler [Version]
availableVersionsFor pkgName = do
  dir <- packageDirFor pkgName
  mresult <- liftIO $ catchDoesNotExist $ do
    files <- getDirectoryContents dir
    return $ mapMaybe (stripSuffix ".json" >=> parseVersion') files
  return $ fromMaybe [] mresult

getPackageModificationTime :: PackageName -> Handler UTCTime
getPackageModificationTime pkgName = do
  dir <- packageDirFor pkgName
  liftIO $ getModificationTime dir

getLatestVersionFor :: PackageName -> Handler (Maybe Version)
getLatestVersionFor pkgName = do
  vs  <- availableVersionsFor pkgName
  return $ map NN.maximum (NN.fromNullable vs)

-- | Insert a package at a specific version into the database.
insertPackage :: D.VerifiedPackage -> Handler ()
insertPackage pkg@D.Package{..} = do
  let pkgName = D.packageName pkg
  file <- packageVersionFileFor pkgName pkgVersion
  clearCache pkgName pkgVersion
  writeFileWithParents file (toStrict (A.encode pkg))

packageDirFor :: PackageName -> Handler String
packageDirFor pkgName = do
  dir <- getDataDir
  return (dir ++ "/verified/" ++ unpack (runPackageName pkgName))

packageVersionFileFor :: PackageName -> Version -> Handler String
packageVersionFileFor pkgName version = do
  dir <- packageDirFor pkgName
  return (dir ++ "/" ++ showVersion version ++ ".json")

decodeVerifiedPackageFile :: String -> ByteString -> Handler D.VerifiedPackage
decodeVerifiedPackageFile filepath contents =
  decodePackageFile filepath contents

-- | Prefer decodeVerifiedPackageFile to this function, where possible.
decodePackageFile :: (A.FromJSON a) => String -> ByteString -> Handler (D.Package a)
decodePackageFile filepath contents = do
  case A.eitherDecodeStrict contents of
    Left err -> do
      $logError (T.pack ("Invalid JSON in: " ++ show filepath ++
                         ", error: " ++ show err))
      sendResponseStatus internalServerError500 ("" :: String)
    Right pkg ->
      return pkg
