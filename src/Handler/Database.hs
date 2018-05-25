-- | This module provides functions for working with the database (i.e. the set
-- of JSON files representing the packages which have been uploaded to
-- Pursuit).
module Handler.Database
  ( getAllPackageNames
  , getAllPackages
  , getLatestPackages
  , lookupPackage
  , availableVersionsFor
  , getLatestVersionFor
  , insertPackage
  , getDependents
  ) where

import Import
import qualified Data.Aeson as A
import qualified Data.NonNull as NN
import qualified Data.Text as T
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))
import Utils
import Database.Persist.Sqlite (createSqlitePool, runSqlPool, sqlDatabase, sqlPoolSize)
import Data.Either.Extra (fromRight')
import Data.Version (Version, showVersion)
import System.Directory
  (getDirectoryContents, getModificationTime, doesDirectoryExist)

import Web.Bower.PackageMeta (PackageName, mkPackageName, runPackageName)  
import qualified Language.PureScript.Docs as D

import Handler.Utils
import Handler.Caching (clearCache)

getAllPackageNames :: Handler [PackageName]
getAllPackageNames = do
  dir <- getDataDir
  contents <- liftIO $ getAllPackageNamesIO dir
  return contents

getDependents :: PackageName -> Handler [PackageName]
getDependents pkgdep = do
  dependents <- runDB $ E.select $ E.distinct $ E.from $ \pkg -> do
    E.where_ (pkg ^. PackageDependency E.==. (E.val depName))
    return (pkg ^. PackageDependent)
  -- I am wary of the fromRight', but hoping that it should be safe because
  -- I saved only verified package names in DB
  return $ map (fromRight' . mkPackageName . E.unValue) dependents
  where depName = runPackageName pkgdep

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

-- | This is horribly inefficient, but it will do for now. Note that this
-- only gets the latest version of each package in the database.
getAllPackages :: Handler [D.VerifiedPackage]
getAllPackages = do
  pkgNames <- getAllPackageNames
  pkgNamesAndVersions <- catMaybes <$> traverse withVersion pkgNames
  catMaybes <$> traverse lookupPackageMay pkgNamesAndVersions
  where
  withVersion name = (map . map) (name,) (getLatestVersionFor name)
  lookupPackageMay = map hush . uncurry lookupPackage


lookupPackage :: PackageName -> Version -> Handler (Either SomethingMissing D.VerifiedPackage)
lookupPackage pkgName version = do
  file <- packageVersionFileFor pkgName version
  mcontents <- liftIO (readFileMay file)
  case mcontents of
    Just contents ->
      Right <$> decodeVerifiedPackageFile file contents
    Nothing -> do
      -- Work out whether there's no such package or just no such version
      dir <- packageDirFor pkgName
      exists <- liftIO $ doesDirectoryExist dir
      return $ Left $ if exists then NoSuchPackageVersion else NoSuchPackage

availableVersionsFor :: PackageName -> Handler [Version]
availableVersionsFor pkgName = do
  dir <- packageDirFor pkgName
  mresult <- liftIO $ catchDoesNotExist $ do
    files <- getDirectoryContents dir
    return $ mapMaybe (stripSuffix ".json" >=> D.parseVersion') files
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
