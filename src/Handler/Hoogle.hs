
module Handler.Hoogle where

import Import
import Control.Category ((>>>))
import qualified Data.Text.Lazy as LT
import Data.Char (chr)
import qualified Hoogle
import qualified Language.PureScript.Docs as D
import qualified Web.Bower.PackageMeta as Bower

import Model.DocsAsHoogle (packageAsHoogle)
import Handler.Database
import Handler.Packages (findPackage, getLatestVersion)
import Handler.Caching (cacheText)
import Handler.Utils

getPackageHoogleR :: PathPackageName -> PathVersion -> Handler LT.Text
getPackageHoogleR (PathPackageName pkgName) (PathVersion version) = 
  cacheText $ findPackage pkgName version (return . packageAsHoogle)

generateDatabase :: Handler Hoogle.Database
generateDatabase = do
  packages <- getAllPackages
  let inputs = map (unpack . packageAsHoogle) packages
  let inputData = concat $ intersperse "\n\n" $ inputs
  outputFile <- (++) <$> getWorkingDirectory <*> getTimestampedFilename "data.tmp.hoo"

  -- Just to ensure that parent directories exist
  writeFileWithParents outputFile ("<test>" :: Text)

  (db, errs) <- liftIO $ createDatabase inputData outputFile
  -- TODO: traverse warn errs
  return db

-- | Gets the directory used as a working directory for database generation.
getWorkingDirectory :: Handler FilePath
getWorkingDirectory = (++ "/hoogle/work/") <$> getDataDir

-- | Get a temporary file name with the given suffix. Eg:
-- "test.txt" -> "20150628.test.txt"
getTimestampedFilename :: (MonadIO m) => String -> m String
getTimestampedFilename suffix = do
  time <- liftIO getCurrentTime
  return $ formatTime defaultTimeLocale "%d%m%y%H%M%S." time ++ suffix

dummyHackageUrl :: String
dummyHackageUrl = "dummy.hackage.url/"

-- | Given a Hoogle result url, try to extract the name of the package and the
-- title of the relevant declaration.
extractPackageAndTitle :: String -> Maybe (Bower.PackageName, String)
extractPackageAndTitle url = (,) <$> extractPackage url <*> pure (extractTitle url)
  where
  extractPackage =
    stripPrefix (dummyHackageUrl ++ "package/")
    >>> map (takeWhile (/= '/'))
    >=> (rightMay . Bower.parsePackageName)

  rightMay (Right x) = Just x
  rightMay _         = Nothing

  extractTitle =
    reverse
    >>> takeWhile (/= '/')
    >>> reverse
    >>> drop 3
    >>> decodeAnchorId

-- | Takes an anchor id (created by haddock-api:Haddock.Utils.makeAnchorId) and
-- returns the string that produced it.
--
-- Eg: "hello" -> "hello", "-33--33-" -> "!!"
decodeAnchorId :: String -> String
decodeAnchorId = go []
  where
  go xs [] = xs
  go xs ('-':ys) =
    let (charCode, rest) = span (/= '-') ys
    in case readMay charCode of
      Just code -> go (xs ++ [chr code]) (drop 1 rest)
      Nothing   -> go (xs ++ ['-']) ys
  go xs (y:ys) = go (xs ++ [y]) ys

searchDatabase :: Hoogle.Database -> String -> Handler [(Bower.PackageName, String)]
searchDatabase db query =
  case Hoogle.parseQuery Hoogle.Haskell query of
    Left err ->
      fail $ show err
    Right q ->
      let results = Hoogle.search db q
      in  return $ mapMaybe munge results
  where
  munge =
    extractPackageAndTitle <=< resultUrl . snd

  resultUrl r =
    case Hoogle.locations r of
      [(x, _)] -> Just x
      _        -> Nothing

createDatabase ::
  String -- ^ Hoogle input data
  -> FilePath -- ^ Output file name
  -> IO (Hoogle.Database, [Hoogle.ParseError])
createDatabase inputData outputFile = do
  errs <- Hoogle.createDatabase dummyHackageUrl Hoogle.Haskell [] inputData outputFile
  db <- Hoogle.loadDatabase outputFile
  return (db, errs)

-- | This is horribly inefficient, but it will do for now.
getAllPackages :: Handler [D.VerifiedPackage]
getAllPackages = do
  pkgNames <- getAllPackageNames
  pkgNamesAndVersions <- catMaybes <$> traverse withVersion pkgNames
  catMaybes <$> traverse (uncurry lookupPackage) pkgNamesAndVersions
  where
  withVersion name = (map . map) (name,) (getLatestVersion name)
