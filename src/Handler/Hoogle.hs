
module Handler.Hoogle where

import Import
import Control.Category ((>>>))
import qualified Data.Text.Lazy as LT
import Data.Char (chr, isAlphaNum)
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

  createDatabase inputData outputFile

-- | Gets the directory used as a working directory for database generation.
getWorkingDirectory :: Handler FilePath
getWorkingDirectory = (++ "/hoogle/work/") <$> getDataDir

-- | Get a temporary file name with the given suffix. Eg:
-- "test.txt" -> "20150628.test.txt"
getTimestampedFilename :: (MonadIO m) => String -> m String
getTimestampedFilename suffix = do
  time <- liftIO getCurrentTime
  return $ formatTime defaultTimeLocale "%Y%m%d-%H%M%S." time ++ suffix

dummyHackageUrl :: String
dummyHackageUrl = "dummy.hackage.url/"

-- | Given a Hoogle result url, try to extract:
--   * the name of the package,
--   * the module that the declaration is in,
--   * the title of the declaration.
--
-- This is quite horrible. Sorry.
extractDeclDetails :: String -> Maybe (Bower.PackageName, String, String)
extractDeclDetails url =
  (,,) <$> extractPackage url
       <*> extractModule url
       <*> pure (extractTitle url)
  where
  extractPackage =
    stripPrefix (dummyHackageUrl ++ "package/")
    >>> map (takeWhile (/= '/'))
    >=> (rightMay . Bower.parsePackageName)

  rightMay (Right x) = Just x
  rightMay _         = Nothing

  extractModule =
    reverse
    >>> dropWhile (/= '#')
    >>> drop 1
    >>> stripPrefix (reverse ".html")
    >>> map (takeWhile (/= '/') >>> reverse >>> map minusToDot)

  minusToDot '-' = '.'
  minusToDot x = x

  extractTitle =
    reverse
    >>> takeWhile (/= '#')
    >>> reverse
    >>> drop 2
    >>> decodeAnchorId
    >>> bracketOperators

  bracketOperators str
    | any (not . isAlphaNum) str = "(" ++ str ++ ")"
    | otherwise = str

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

searchDatabase :: Hoogle.Database -> String -> Handler [(Bower.PackageName, String, String)]
searchDatabase db query =
  case Hoogle.parseQuery Hoogle.Haskell query of
    Left err ->
      fail $ show err
    Right q ->
      let results = Hoogle.search db q
      in  return $ mapMaybe munge results
  where
  munge =
    extractDeclDetails <=< resultUrl . snd

  resultUrl r =
    case Hoogle.locations r of
      [(x, _)] -> Just x
      _        -> Nothing

createDatabase ::
  String -- ^ Hoogle input data
  -> FilePath -- ^ Output file name
  -> Handler Hoogle.Database
createDatabase inputData outputFile = do
  (db, errs) <- liftIO $ do
    errs <- Hoogle.createDatabase dummyHackageUrl Hoogle.Haskell [] inputData outputFile
    db <- Hoogle.loadDatabase outputFile
    return (db, errs)

  unless (null errs) $ do
    errorsFile <- getFilename "errors.txt"
    writeFile errorsFile (unlines $ map tshow errs)

    inputFile  <- getFilename "input.txt"
    writeFile inputFile inputData

    $logWarn ("Hoogle database regeneration produced " <>
              tshow (length errs) <> " warnings, see " <> pack errorsFile <>
              " for details.")

  return db
  where
  getFilename suffix = (++) <$> getWorkingDirectory
                            <*> getTimestampedFilename suffix

-- | This is horribly inefficient, but it will do for now.
getAllPackages :: Handler [D.VerifiedPackage]
getAllPackages = do
  pkgNames <- getAllPackageNames
  pkgNamesAndVersions <- catMaybes <$> traverse withVersion pkgNames
  catMaybes <$> traverse (uncurry lookupPackage) pkgNamesAndVersions
  where
  withVersion name = (map . map) (name,) (getLatestVersion name)
