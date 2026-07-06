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
import qualified Control.Concurrent.STM as STM
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
          withLargeDecodeBudget policy size (decodeFrom file)
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
  -- The file is read inside the budget (rather than before queueing for it)
  -- so that threads waiting their turn do not each pin a copy of a large
  -- file's bytes. The decoded package is forced before the budget share is
  -- released; the decode is eager in practice, but we make certain the
  -- allocation spike stays inside the budget.
  decodeFrom :: String -> Handler (Maybe D.VerifiedPackage)
  decodeFrom file = do
    mcontents <- liftIO (readFileMay file)
    case mcontents of
      Nothing -> return Nothing
      Just contents -> do
        pkg <- decodeVerifiedPackageFile file contents
        pkg `deepseq` return (Just pkg)

-- | Decoding a package's docs JSON transiently needs tens of times the file's
-- size in memory, so concurrent requests for (rarely cached, because there
-- are so many of them) documentation pages of packages with large files can
-- exhaust the heap. Serialising only the decodes of files of 5MB or more
-- turned out not to be enough: a crawler fetching many pages of a package
-- whose file sits just below whatever cutoff is chosen (next-purs-rsc, at
-- 4.4MB, has ~1,900 module pages) stacks enough concurrent decodes to
-- exhaust the heap anyway. Decodes of large files therefore share an
-- aggregate budget: a decode is admitted once the total size of large files
-- being decoded fits within 'largeDecodeBudget', or when no other large
-- decode is running (so a single file bigger than the whole budget is still
-- served, by itself). Packages below the threshold - nearly all of them -
-- are unaffected.
--
-- The budget is measured in file bytes as a proxy for heap use, and it
-- bounds the decode spike, not the decoded package, which lives on a little
-- longer while the page renders. Admission is not first-come-first-served: a
-- large file's decode can be overtaken by smaller ones that fit the
-- remaining budget, which is acceptable because waiters are bounded and
-- contention is rare.
--
-- The queue is bounded: once 'maxLargeDecodeWaiters' threads hold or await a
-- share of the budget, further requests fail fast with a 503 rather than
-- accumulating without limit while clients time out. The hourly search index
-- regeneration ('WaitWhenBusy') is exempt from the bound - it must not
-- silently omit a package from the index just because the server is busy, and
-- as a single sequential thread it adds at most one waiter.
withLargeDecodeBudget :: LargeDecodePolicy -> Integer -> Handler a -> Handler a
withLargeDecodeBudget policy size action = do
  app <- getYesod
  let counter = appLargeDecodeWaiters app
  let inFlight = appDecodeBytesInFlight app
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
      bracket_ (acquireBudget inFlight) (releaseBudget inFlight) action
        `finally` atomically (modifyTVar' counter (subtract 1))
    else
      sendResponseStatus serviceUnavailable503
        ("Too many concurrent requests for large packages; try again shortly." :: Text)
  where
  maxLargeDecodeWaiters = 16 :: Int

  acquireBudget inFlight = atomically $ do
    inUse <- readTVar inFlight
    STM.check (inUse == 0 || inUse + size <= largeDecodeBudget)
    writeTVar inFlight (inUse + size)

  releaseBudget inFlight =
    atomically (modifyTVar' inFlight (subtract size))

-- | Files at least this large count against 'largeDecodeBudget' while they
-- are being decoded.
largeDecodeThreshold :: Integer
largeDecodeThreshold = 1024 * 1024

-- | The maximum total size of large package files being decoded at any one
-- time.
largeDecodeBudget :: Integer
largeDecodeBudget = 16 * 1024 * 1024

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
