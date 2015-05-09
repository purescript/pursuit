
module Model.Database
  ( lookupPackage
  , availableVersionsFor
  , insertPackage
  ) where

import Import
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Version (Version, showVersion)
import System.Directory (doesDirectoryExist, getDirectoryContents,
                         createDirectoryIfMissing)
import System.FilePath (takeDirectory)

import Web.Bower.PackageMeta (PackageName, runPackageName)
import qualified Language.PureScript.Docs as D

lookupPackage :: PackageName -> Version -> Handler (Maybe D.VerifiedPackage)
lookupPackage pkgName version = do
  file <- packageVersionFileFor pkgName version
  mcontents <- liftIO $ catchJust selectDoesNotExist
                                  (Just <$> BL.readFile file)
                                  (const (return Nothing))
  case mcontents of
    Nothing       -> return Nothing
    Just contents -> Just <$> decodePackageFile file contents

  where
  selectDoesNotExist e
    | isDoesNotExistErrorType (ioeGetErrorType e) = Just ()
    | otherwise = Nothing

availableVersionsFor :: PackageName -> Handler (Maybe [Version])
availableVersionsFor pkgName = do
  dir <- packageDirFor pkgName
  exists <- liftIO (doesDirectoryExist dir)
  if (not exists)
    then return Nothing
    else do
      files <- liftIO (getDirectoryContents dir)
      return $ Just $ mapMaybe (stripSuffix ".json" >=> D.parseVersion') files

-- | Insert a package at a specific version
insertPackage :: D.VerifiedPackage -> Handler ()
insertPackage pkg@D.Package{..} = do
  let pkgName = D.packageName pkg
  file <- packageVersionFileFor pkgName pkgVersion
  liftIO $ do
    createDirectoryIfMissing True (takeDirectory file)
    BL.writeFile file (A.encode pkg)

getDataDir :: Handler String
getDataDir = appDataDir . appSettings <$> getYesod

packageDirFor :: PackageName -> Handler String
packageDirFor pkgName = do
  dir <- getDataDir
  return (dir ++ "/" ++ runPackageName pkgName)

packageVersionFileFor :: PackageName -> Version -> Handler String
packageVersionFileFor pkgName version = do
  dir <- packageDirFor pkgName
  return (dir ++ "/" ++ showVersion version ++ ".json")

decodePackageFile :: String -> BL.ByteString -> Handler D.VerifiedPackage
decodePackageFile filepath contents = do
  case A.eitherDecode contents of
    Left err -> do
      $logError (T.pack ("Invalid JSON in: " ++ show filepath ++
                         ", error: " ++ show err))
      sendResponseStatus internalServerError500 ("" :: String)
    Right pkg ->
      return pkg
