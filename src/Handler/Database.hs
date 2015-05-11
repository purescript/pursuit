
module Handler.Database
  ( lookupPackage
  , availableVersionsFor
  , insertPackage
  , insertPendingVerification
  , verifyPackage
  , lookupPendingPackage
  , VerifyResult(..)
  , generateKey
  ) where

import Import
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Version (Version, showVersion)
import qualified Data.ByteString.Base64.URL as Base64
import System.Directory (doesDirectoryExist, getDirectoryContents,
                        removeFile, getModificationTime)
import System.FilePath (takeDirectory)

import Web.Bower.PackageMeta (PackageName, runPackageName)
import qualified Language.PureScript.Docs as D

import Handler.Utils
import Handler.Caching (clearCache)

lookupPackage :: PackageName -> Version -> Handler (Maybe D.VerifiedPackage)
lookupPackage pkgName version = do
  file <- packageVersionFileFor pkgName version
  mcontents <- liftIO (readFileMay file)
  case mcontents of
    Nothing       -> return Nothing
    Just contents -> Just <$> decodeVerifiedPackageFile file contents

availableVersionsFor :: PackageName -> Handler (Maybe [Version])
availableVersionsFor pkgName = do
  dir <- packageDirFor pkgName
  exists <- liftIO (doesDirectoryExist dir)
  if (not exists)
    then return Nothing
    else do
      files <- liftIO (getDirectoryContents dir)
      return $ Just $ mapMaybe (stripSuffix ".json" >=> D.parseVersion') files

-- | Insert a package at a specific version into the database.
insertPackage :: D.VerifiedPackage -> Handler ()
insertPackage pkg@D.Package{..} = do
  let pkgName = D.packageName pkg
  file <- packageVersionFileFor pkgName pkgVersion
  clearCache pkgName pkgVersion
  liftIO $ writeFileWithParents file (A.encode pkg)

-- | In order to prevent denial of service by filling up the pending packages
-- directory, we set a limit on the number of pending packages which are
-- allowed to exist at one time.
maxPendingPackages :: Int
maxPendingPackages = 100

-- | Insert an uploaded package, pending verification. The second argument
-- is the verification key, which is also used as the file name.
insertPendingVerification :: D.UploadedPackage -> VerificationKey -> Handler ()
insertPendingVerification pkg key = do
  file <- pendingVerificationFileFor key
  let dir = takeDirectory file
  liftIO $ writeFileWithParents file (A.encode pkg)
  removeLeastRecentlyWritten dir (maxPendingPackages - 1)

lookupPendingPackage :: VerificationKey -> Handler (Maybe D.UploadedPackage)
lookupPendingPackage key = do
  file <- pendingVerificationFileFor key
  mcontents <- liftIO (readFileMay file)
  case mcontents of
    Nothing       -> return Nothing
    Just contents -> Just <$> decodeUploadedPackageFile file contents

verifyPackage :: VerificationKey -> D.GithubUser -> Handler VerifyResult
verifyPackage key user = do
  file <- pendingVerificationFileFor key
  mcontents <- liftIO (readFileMay file)
  case mcontents of
    Nothing -> return VerifyUnknownKey
    Just contents -> do
      pkg <- decodeUploadedPackageFile file contents
      let pkg' = D.verifyPackage user pkg
      insertPackage pkg'
      liftIO $ removeFile file
      return (VerifySuccess pkg')

packageDirFor :: PackageName -> Handler String
packageDirFor pkgName = do
  dir <- getDataDir
  return (dir ++ "/verified/" ++ runPackageName pkgName)

packageVersionFileFor :: PackageName -> Version -> Handler String
packageVersionFileFor pkgName version = do
  dir <- packageDirFor pkgName
  return (dir ++ "/" ++ showVersion version ++ ".json")

pendingVerificationFileFor :: VerificationKey -> Handler String
pendingVerificationFileFor key = do
  dir <- getDataDir
  return (dir ++ "/uploaded/" ++ BC8.unpack (runVerificationKey key) ++ ".json")

decodeVerifiedPackageFile :: String -> BL.ByteString -> Handler D.VerifiedPackage
decodeVerifiedPackageFile filepath contents =
  decodePackageFile filepath contents

decodeUploadedPackageFile :: String -> BL.ByteString -> Handler D.UploadedPackage
decodeUploadedPackageFile filepath contents =
  decodePackageFile filepath contents

-- | Prefer decodeVerifiedPackageFile or decodeUploadedPackageFile to this
-- function.
decodePackageFile :: (A.FromJSON a) => String -> BL.ByteString -> Handler (D.Package a)
decodePackageFile filepath contents = do
  case A.eitherDecode contents of
    Left err -> do
      $logError (T.pack ("Invalid JSON in: " ++ show filepath ++
                         ", error: " ++ show err))
      sendResponseStatus internalServerError500 ("" :: String)
    Right pkg ->
      return pkg

-- | Given a directory, delete the oldest files until there are no more than
-- the supplied number of files remaining.
removeLeastRecentlyWritten :: String -> Int -> Handler ()
removeLeastRecentlyWritten dir maxCount = do
  contents <- liftIO $ getDirectoryContents' dir
  let len = length contents
  when (len > maxCount) $ do
    withMTimes <- liftIO $ mapM withModificationTime contents
    let sorted = sort withMTimes
    forM_ (take (len - maxCount) sorted) $ \(_, f) -> do
      $logInfo ("Deleting least recently used pending package: " ++ pack f)
      liftIO $ removeFile f
  where
  withModificationTime f = (,) <$> getModificationTime f <*> pure f

  -- like getDirectoryContents, but including the directory, and without
  -- "." or ".."
  getDirectoryContents' d =
    map (prefixedBy d) . filterDots <$> getDirectoryContents d
  filterDots =
    filter (\x -> x `onotElem` [".", ".."])
  prefixedBy d f =
    d ++ "/" ++ f

------------------------
-- Verification

data VerifyResult
  = VerifySuccess D.VerifiedPackage
  | VerifyUnknownKey
  deriving (Show, Eq, Ord)

keyLength :: Int
keyLength = 60

generateKey :: Handler VerificationKey
generateKey =
  VerificationKey . Base64.encode <$> generateBytes keyLength
