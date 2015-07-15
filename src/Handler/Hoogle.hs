
module Handler.Hoogle
  ( getPackageHoogleR
  , getSearchR
  , generateDatabase
  , getDatabase
  , searchDatabase
  , HoogleResult(..)
  ) where

import Import
import Control.Category ((>>>))
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Concurrent (forkIO)
import qualified Data.Text.Lazy as LT
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Char (chr, isAlphaNum)
import Data.Version (Version, showVersion)
import qualified Hoogle
import qualified Language.PureScript.Docs as D
import qualified Web.Bower.PackageMeta as Bower
import System.Directory (removeDirectoryRecursive)

import Model.DocsAsHoogle (packageAsHoogle)
import Model.DocsAsHtml (makeFragment)
import Handler.Database
import Handler.Packages (findPackage)
import Handler.Caching (cacheText)
import Handler.Utils
import TemplateHelpers (tagStrToHtml, getFragmentRender)
import TimeUtils (oneDay, getElapsedTimeSince)

getPackageHoogleR :: PathPackageName -> PathVersion -> Handler LT.Text
getPackageHoogleR (PathPackageName pkgName) (PathVersion version) =
  cacheText $ findPackage pkgName version (return . packageAsHoogle)

getSearchR :: Handler TypedContent
getSearchR = do
  mquery <- (map . map) unpack $ lookupGetParam "q"
  case mquery of
    Nothing -> redirect HomeR
    Just query -> do
      db <- getDatabase
      results <- runExceptT $ searchDatabase db query
      selectRep $ do
        provideRep (htmlOutput query results)
        provideRep (jsonOutput results)

  where
  htmlOutput :: String -> Either String [HoogleResult] -> Handler Html
  htmlOutput query results = do
    fr <- getFragmentRender
    content <- defaultLayout $(widgetFile "search")
    let status = either (const badRequest400) (const ok200) results
    sendResponseStatus status content

  jsonOutput results = do
    case results of
      Left err ->
        sendResponseStatus badRequest400 $
          object [ "error" .= err ]
      Right rs ->
        toJSON <$> traverse hoogleResultToJSON rs

getDatabase :: Handler Hoogle.Database
getDatabase = do
  foundation <- getYesod
  let dbVar = appHoogleDatabase foundation
  (lastGenTime, db) <- readVar dbVar
  age <- liftIO $ getElapsedTimeSince lastGenTime
  let maxAge = appHoogleDatabaseMaxAge $ appSettings foundation
  if (age < maxAge)
    then return db
    else do
      $logInfo "Regenerating Hoogle database..."
      runInnerHandler <- handlerToIO
      _ <- liftIO $ forkIO $ runInnerHandler regenerateDatabase
      -- For now, return the old db. We don't want to be too slow.
      return db
  where
  readVar = liftIO . readTVarIO

regenerateDatabase :: Handler ()
regenerateDatabase = do
  generateDatabase >>= maybe (return ()) storeNewDatabase

generateDatabase :: Handler (Maybe Hoogle.Database)
generateDatabase = do
  packages <- getAllPackages
  let inputs = map (unpack . packageAsHoogle) packages
  let inputData = concat $ intersperse "\n\n" $ inputs
  outputFile <- (++) <$> getWorkingDirectory <*> getTimestampedFilename "data.tmp.hoo"

  -- Just to ensure that parent directories exist
  writeFileWithParents outputFile ("<test>" :: Text)

  createDatabase inputData outputFile

-- | Given a freshly generated database, store it in the foundation.
storeNewDatabase :: Hoogle.Database -> Handler ()
storeNewDatabase db = do
  now <- liftIO getCurrentTime
  dbVar <- appHoogleDatabase <$> getYesod
  liftIO $ atomically $ writeTVar dbVar (now, db)

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

-- | Given a Hoogle result 'self' TagStr, a package version, and result url,
-- try to extract a HoogleResult.  The url is the source of:
--   * the name of the package,
--   * the module that the declaration is in,
--   * the title of the declaration.
--
-- This is quite horrible. Sorry.
extractHoogleResult :: Hoogle.TagStr -> String -> ExceptT String Handler HoogleResult
extractHoogleResult tagStr url = do
  let mpkgName = extractPackage url
  pkgName <- justOr' ("Unable to determine package name: " ++ url) mpkgName

  version <- lift (getLatestVersionFor pkgName)
                >>= justOr' ("Unable to get latest version for " ++
                             Bower.runPackageName pkgName)

  return $
    HoogleResult pkgName
                 version
                 tagStr
                 (extractInfo url)
  where
  justOr' msg = justOr $ "extractHoogleResult: " ++ msg

  extractInfo u =
    case (extractModule u, extractTitle u) of
      (Nothing,      _         ) -> PackageResult
      (Just modName, Nothing   ) -> ModuleResult modName
      (Just modName, Just title) -> DeclarationResult modName title

  extractPackage =
    stripPrefix (dummyHackageUrl ++ "package/")
    >>> map (takeWhile (/= '/'))
    >=> (rightMay . Bower.parsePackageName)

  rightMay (Right x) = Just x
  rightMay _         = Nothing

  extractModule :: String -> Maybe String
  extractModule =
    reverse
    >>> if' ('#' `elem`) (dropWhile (/= '#') >>> drop 1)
    >>> stripPrefix (reverse ".html")
    >>> map (takeWhile (/= '/') >>> reverse >>> map minusToDot)

  if' f g x
    | f x       = g x
    | otherwise = x

  minusToDot '-' = '.'
  minusToDot x = x

  extractTitle u =
    guard ('#' `elem` u) >> Just (go u)
    where
    go = reverse
         >>> takeWhile (/= '#')
         >>> reverse
         >>> drop 2
         >>> decodeAnchorId
         >>> bracketOperators

  bracketOperators str
    | any isAlphaNum str = str
    | otherwise = "(" ++ str ++ ")"

justOr :: (Monad m) => e -> Maybe a -> ExceptT e m a
justOr err = maybe (throwE err) return

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

-- | A single result from a Hoogle query.
data HoogleResult = HoogleResult
  { hrPkgName    :: Bower.PackageName
  , hrPkgVersion :: Version
  , hrTagStr     :: Hoogle.TagStr
  , hrInfo       :: HoogleResultInfo
  }
  deriving (Show, Eq)

data HoogleResultInfo
  = PackageResult
  | ModuleResult      String -- ^ Module name
  | DeclarationResult String String -- ^ Module name & declaration title
  deriving (Show, Eq)

hoogleResultToJSON :: HoogleResult -> Handler Value
hoogleResultToJSON result@HoogleResult{..} = do
    url <- getFragmentRender <*> pure (routeResult result)
    return $
      object [ "package" .= hrPkgName
             , "version" .= showVersion hrPkgVersion
             , "markup" .= renderHtml (tagStrToHtml hrTagStr)
             , "text" .= Hoogle.showTagText hrTagStr
             , "info" .= toJSON hrInfo
             , "url" .= url
             ]

instance ToJSON HoogleResultInfo where
  toJSON i = object $ case i of
    PackageResult ->
      [ "type" .= ("package" :: Text)
      ]
    ModuleResult moduleName ->
      [ "type" .= ("module" :: Text)
      , "module" .= moduleName
      ]
    DeclarationResult moduleName declTitle ->
      [ "type" .= ("declaration" :: Text)
      , "module" .= moduleName
      , "title" .= declTitle
      ]

routeResult :: HoogleResult -> ((Route App), Maybe Text)
routeResult HoogleResult{..} =
  case hrInfo of
    PackageResult ->
      ( PackageR ppkgName
      , Nothing
      )
    ModuleResult modName ->
      ( PackageVersionModuleDocsR ppkgName pversion modName
      , Nothing
      )
    DeclarationResult modName declTitle ->
      ( PackageVersionModuleDocsR ppkgName pversion modName
      , Just $ pack $ drop 1 $ makeFragment declTitle
      )
  where
  ppkgName = PathPackageName hrPkgName
  pversion = PathVersion hrPkgVersion

searchDatabase :: Hoogle.Database -> String -> ExceptT String Handler [HoogleResult]
searchDatabase db query = do
  q <- either (throwE . show) return $ parse query
  let results = Hoogle.search db q
  actuals <- concat <$> traverse (munge . snd) results
  extras <- lift $ searchForPackage query
  return $ extras ++ actuals

  where
  parse = Hoogle.parseQuery Hoogle.Haskell

  munge result = do
    let tagStr    = Hoogle.self result
    let locations = Hoogle.locations result
    traverse (extractHoogleResult tagStr) (map fst locations)

-- | Search the package database for a particular package manually, and
-- construct a HoogleResult (as if Hoogle had performed the search). For some
-- reason, package searching with Hoogle isn't working right now, so this will
-- do in the meantime.
searchForPackage :: String -> Handler [HoogleResult]
searchForPackage (toLower -> query) = do
  (++) <$> go query <*> go ("purescript-" ++ query)
  where
  go q = map maybeToList $ runMaybeT $ do
    pkgName <- MaybeT $ return $ either (const Nothing) Just $ Bower.parsePackageName q
    version <- MaybeT $ getLatestVersionFor pkgName
    return $
      HoogleResult pkgName
                   version
                   (packageTagStr pkgName)
                   PackageResult

  packageTagStr pkgName =
    mconcat
      [ Hoogle.TagEmph (s "package")
      , s " "
      , Hoogle.TagBold $ Hoogle.TagEmph $ s $ Bower.runPackageName pkgName
      ]
    where
    s = Hoogle.Str

createDatabase ::
  String -- ^ Hoogle input data
  -> FilePath -- ^ Output file name
  -> Handler (Maybe Hoogle.Database)
createDatabase inputData outputFile = do
  (db, errs) <- liftIO $ do
    errs <- Hoogle.createDatabase dummyHackageUrl Hoogle.Haskell [] inputData outputFile
    db <- Hoogle.loadDatabase outputFile
    return (db, errs)

  maxErrors <- appMaxHoogleParseErrors . appSettings <$> getYesod
  handleErrors db errs maxErrors

  where
  handleErrors db errs maxErrors = do
    let countErrors = length errs
    workDir <- getWorkingDirectory
    if countErrors == 0
      then do
        $logInfo ("Hoogle database regeneration complete, no errors.")
        -- Clean up
        liftIO $ removeDirectoryRecursive workDir
        return (Just db)

      else do
        -- Clean up older files
        deleteFilesOlderThan oneDay workDir

        -- Log errors and input, for diagnostic purposes
        errorsFile <- getFilename "errors.txt"
        writeFile errorsFile (unlines $ map tshow errs)

        inputFile  <- getFilename "input.txt"
        writeFile inputFile inputData

        let msg = logMsg countErrors errorsFile
        if (countErrors < maxErrors)
          then do
            $logWarn msg
            return (Just db)
          else do
            $logError msg
            return Nothing

  logMsg countErrors errorsFile =
    "Hoogle database regeneration produced " <> tshow countErrors <> " errors,"
    <> " see " <> pack errorsFile <> " for details."

  getFilename suffix = (++) <$> getWorkingDirectory
                            <*> getTimestampedFilename suffix

-- | This is horribly inefficient, but it will do for now.
getAllPackages :: Handler [D.VerifiedPackage]
getAllPackages = do
  pkgNames <- getAllPackageNames
  pkgNamesAndVersions <- catMaybes <$> traverse withVersion pkgNames
  catMaybes <$> traverse lookupPackageMay pkgNamesAndVersions
  where
  withVersion name = (map . map) (name,) (getLatestVersionFor name)
  lookupPackageMay = map (either (const Nothing) Just) . uncurry lookupPackage
