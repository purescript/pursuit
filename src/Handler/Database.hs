
module Handler.Database
  ( getAllPackageNames
  , lookupPackage
  , availableVersionsFor
  , getLatestVersionFor
  , insertPackage
  ) where

import Import
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Version (Version, showVersion)
import System.Directory (getDirectoryContents)

import Web.Bower.PackageMeta (PackageName, mkPackageName, runPackageName)
import qualified Language.PureScript.Docs as D

import Handler.Utils
import Handler.Caching (clearCache)

getAllPackageNames :: Handler [PackageName]
getAllPackageNames = do
  dir <- getDataDir
  contents <- liftIO $ getDirectoryContents (dir ++ "/verified/")
  return $ rights $ map mkPackageName contents

lookupPackage :: PackageName -> Version -> Handler (Maybe D.VerifiedPackage)
lookupPackage pkgName version = do
  file <- packageVersionFileFor pkgName version
  mcontents <- liftIO (readFileMay file)
  case mcontents of
    Nothing       -> return Nothing
    Just contents -> Just <$> decodeVerifiedPackageFile file contents

availableVersionsFor :: PackageName -> Handler [Version]
availableVersionsFor pkgName = do
  dir <- packageDirFor pkgName
  mresult <- liftIO $ catchDoesNotExist $ do
    files <- getDirectoryContents dir
    return $ mapMaybe (stripSuffix ".json" >=> D.parseVersion') files
  return $ fromMaybe [] mresult

getLatestVersionFor :: PackageName -> Handler (Maybe Version)
getLatestVersionFor pkgName = do
  vs  <- availableVersionsFor pkgName
  let vs' = toMinLen vs :: Maybe (MinLen One [Version])
  return $ map maximum vs'

-- | Insert a package at a specific version into the database.
insertPackage :: D.VerifiedPackage -> Handler ()
insertPackage pkg@D.Package{..} = do
  let pkgName = D.packageName pkg
  file <- packageVersionFileFor pkgName pkgVersion
  clearCache pkgName pkgVersion
  writeFileWithParents file (A.encode pkg)

packageDirFor :: PackageName -> Handler String
packageDirFor pkgName = do
  dir <- getDataDir
  return (dir ++ "/verified/" ++ runPackageName pkgName)

packageVersionFileFor :: PackageName -> Version -> Handler String
packageVersionFileFor pkgName version = do
  dir <- packageDirFor pkgName
  return (dir ++ "/" ++ showVersion version ++ ".json")

decodeVerifiedPackageFile :: String -> BL.ByteString -> Handler D.VerifiedPackage
decodeVerifiedPackageFile filepath contents =
  decodePackageFile filepath contents

-- | Prefer decodeVerifiedPackageFile to this function, where possible.
decodePackageFile :: (A.FromJSON a) => String -> BL.ByteString -> Handler (D.Package a)
decodePackageFile filepath contents = do
  case A.eitherDecode contents of
    Left err -> do
      $logError (T.pack ("Invalid JSON in: " ++ show filepath ++
                         ", error: " ++ show err))
      sendResponseStatus internalServerError500 ("" :: String)
    Right pkg ->
      return pkg
