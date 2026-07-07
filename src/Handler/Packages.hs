module Handler.Packages where

import Import
import Text.Blaze (ToMarkup, toMarkup)
import qualified Data.Char as Char
import Data.Version
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Language.PureScript.CoreFn.FromJSON (parseVersion')
import qualified Language.PureScript.Docs as D
import Web.Bower.PackageMeta (PackageName, runPackageName, bowerDependencies, bowerLicense)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.BetterErrors as ABE
import qualified Language.PureScript as P

import SearchIndex (isDeprecated)
import Handler.Database
import Handler.Caching
import Handler.Utils
import TemplateHelpers
import qualified GithubAPI

newtype FirstLetter = FirstLetter { getFirstLetter :: Char }

instance ToMarkup FirstLetter where
  toMarkup (FirstLetter a)
    | Char.isAlpha a = toMarkup (Char.toUpper a)
    | otherwise = toMarkup '#' -- Symbols, digits, etc.

instance Eq FirstLetter where
  FirstLetter a == FirstLetter b
    | Char.isAlpha a && Char.isAlpha b = Char.toUpper a == Char.toUpper b
    | otherwise = True

getHomeR :: Handler Html
getHomeR =
  cacheHtml $ do
    pkgNames <- getAllPackageNames
    latest <- getLatestPackages
    let firstLetter :: PackageName -> Maybe FirstLetter
        firstLetter = fmap FirstLetter . headMay . stripIntro . runPackageName

        stripIntro :: Text -> Text
        stripIntro s = fromMaybe s (stripPrefix "purescript-" s)

        pkgNamesByLetter :: [[PackageName]]
        pkgNamesByLetter = groupBy ((==) `on` (firstLetter )) pkgNames
    defaultLayout $(widgetFile "homepage")

latestVersionOr404 :: PackageName -> Handler Version
latestVersionOr404 pkgName = do
  v <- getLatestVersionFor pkgName
  case v of
    Nothing -> packageNotFound pkgName
    Just v' -> pure v'

getPackageR :: PathPackageName -> Handler Html
getPackageR ppkgName@(PathPackageName pkgName) = do
  v <- latestVersionOr404 pkgName
  redirect (PackageVersionR ppkgName (PathVersion v))

getPackageModuleDocsR :: PathPackageName -> Text -> Handler Html
getPackageModuleDocsR ppkgName@(PathPackageName pkgName) mnString = do
  v <- latestVersionOr404 pkgName
  redirect (PackageVersionModuleDocsR ppkgName (PathVersion v) mnString)

getPackageAvailableVersionsR :: PathPackageName -> Handler Value
getPackageAvailableVersionsR (PathPackageName pkgName) =
  cacheJSON $ do
    renderUrl <- getUrlRender
    vs <- availableVersionsFor pkgName
    let toPair v = [ toJSON $ showVersion v
                   , toJSON $ renderUrl $ alternateVersionUrl v
                   ]
    return $ toJSON $ map toPair $ sort vs
  where
  alternateVersionUrl v = PackageVersionR (PathPackageName pkgName) (PathVersion v)

getPackageVersionR :: PathPackageName -> PathVersion -> Handler Html
getPackageVersionR (PathPackageName pkgName) (PathVersion version) =
  cacheHtmlConditional $
    findPackageWithDeprecation pkgName version $ \pkg@D.Package{..} deprecated -> do
      moduleList <- renderModuleList pkg
      ereadme    <- tryGetReadme pkg
      let cacheStatus = either (const NotOkToCache) (const OkToCache) ereadme
      content <- defaultLayout $ do
        setTitle (toHtml (runPackageName pkgName))
        let dependencies = bowerDependencies pkgMeta
        $(widgetFile "packageVersion")
      return (cacheStatus, content)

isPurescriptPackage :: PackageName -> Bool
isPurescriptPackage pkgName =
    "purescript-" `T.isPrefixOf` runPackageName pkgName

getPackageIndexR :: Handler TypedContent
getPackageIndexR =
  selectRep $ do
    provideRep (redirect HomeR :: Handler Html)
    provideRep . cacheText . map unlines $ pkgNames
    provideRep . cacheJSON . map toJSON  $ pkgNames
  where
  pkgNames :: Handler [TL.Text]
  pkgNames = map (fromStrict . runPackageName) <$> getAllPackageNames

postPackageIndexR :: Handler Value
postPackageIndexR = do
  package <- getUploadedPackageFromBody
  mtoken  <- lookupAuthTokenHeader
  case mtoken of
    Nothing -> notAuthenticated
    Just token -> do
      user <- getUserOrNotAuthenticated token
      let package' = D.verifyPackage user package
      insertPackage package'
      sendResponseCreated $ packageRoute package'

  where
  getUploadedPackageFromBody = do
    ejson <- parseJsonBodyPotentiallyGzipped
    case ejson of
      Left err ->
        badRequest (pack err)
      Right json -> do
        epackage <- parseUploadedPackage json
        case epackage of
          Left err ->
            let errorMessage = unlines $ displayJsonError json err
            in badRequest errorMessage
          Right package ->
            return package

  getUserOrNotAuthenticated token = do
    euser <- GithubAPI.getUser token
    case euser of
      Left err ->
        $logError (tshow err) >> internalServerError
      Right Nothing ->
        notAuthenticated
      Right (Just user) ->
        return user

  lookupAuthTokenHeader = do
    mheader <- lookupHeader "Authorization"
    return $ mheader >>= extractToken

  extractToken header =
    case words (decodeUtf8 header) of
      ["token", token] -> Just $ GithubAuthToken $ encodeUtf8 token
      _ -> Nothing

getPackageVersionDocsR :: PathPackageName -> PathVersion -> Handler Html
getPackageVersionDocsR (PathPackageName pkgName) (PathVersion version) =
  findPackage pkgName version $ \pkg ->
    redirect (packageRoute pkg)

getPackageVersionModuleDocsR :: PathPackageName -> PathVersion -> Text -> Handler Html
getPackageVersionModuleDocsR (PathPackageName pkgName) (PathVersion version) mnString =
  cacheHtml $ findPackageWithDeprecation pkgName version $ \pkg@D.Package{..} deprecated -> do
    moduleList <- renderModuleList pkg
    mhtmlDocs <- renderHtmlDocs pkg mnString
    case mhtmlDocs of
      Nothing -> notFound
      Just htmlDocs ->
        defaultLayout $ do
          let mn = P.moduleNameFromString mnString
          setTitle (toHtml (mnString <> " - " <> runPackageName pkgName))
          $(widgetFile "packageVersionModuleDocs")

getBuiltinDocsR :: Text -> Handler Html
getBuiltinDocsR mnString =
  let
    mn = P.moduleNameFromString mnString
  in
    case primDocsFor mn of
      Just htmlDocs -> do
        moduleList <- builtinModuleList
        defaultLayout $ do
          setTitle (toHtml mnString)
          [whamlet|
            <div .page-title.clearfix>
              <div .page-title__label>Module
              <h1 .page-title__title>#{insertBreaks mn}

            <div .col.col--main>
              #{htmlDocs}

            <div .col.col--aside>
              #{moduleList}
            |]
      Nothing ->
        defaultLayout404 $ [whamlet|
          <h2>Module not found
          <p>No such builtin module: #
            <b>#{mnString}
          |]

findPackage ::
  PackageName ->
  Version ->
  (D.VerifiedPackage -> Handler r) ->
  Handler r
findPackage pkgName version cont = do
  pkg' <- lookupPackage pkgName version
  case pkg' of
    Right pkg -> cont pkg
    Left NoSuchPackage -> packageNotFound pkgName
    Left NoSuchPackageVersion -> packageVersionNotFound pkgName version

findPackageWithDeprecation ::
  PackageName ->
  Version ->
  (D.VerifiedPackage -> Bool -> Handler r) ->
  Handler r
findPackageWithDeprecation pkgName version cont = do
  findPackage pkgName version $ \pkg -> do
    latestVersion <- fromMaybe version <$> getLatestVersionFor pkgName
    deprecated <-
      if latestVersion == version
        then return (isDeprecated pkg)
        else getLatestDeprecation pkgName latestVersion (isDeprecated pkg)
    cont pkg deprecated

data DeprecationMarker = DeprecationMarker
  { markerLatestVersion :: Text
  , markerDeprecated :: Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON DeprecationMarker where
  toJSON DeprecationMarker{..} =
    object
      [ "latest" .= markerLatestVersion
      , "deprecated" .= markerDeprecated
      ]

instance FromJSON DeprecationMarker where
  parseJSON = Aeson.withObject "DeprecationMarker" $ \obj ->
    DeprecationMarker
      <$> obj .: "latest"
      <*> obj .: "deprecated"

getLatestDeprecation :: PackageName -> Version -> Bool -> Handler Bool
getLatestDeprecation pkgName latestVersion fallbackDeprecated = do
  path <- deprecationMarkerFileFor pkgName
  emarker <- tryAny (liftIO (readFileMay path))
  case emarker of
    Right mmarker ->
      case mmarker >>= Aeson.decodeStrict of
        Just DeprecationMarker{..}
          | markerLatestVersion == latestVersionText ->
              return markerDeprecated
        _ ->
          recompute path
    _ -> recompute path
  where
  latestVersionText = pack (showVersion latestVersion)

  recompute path = do
    latestPkg <- hush <$> lookupPackage pkgName latestVersion
    let deprecated = maybe fallbackDeprecated isDeprecated latestPkg
    writeDeprecationMarker path deprecated
    return deprecated

  writeDeprecationMarker path deprecated = do
    let marker =
          DeprecationMarker
            { markerLatestVersion = latestVersionText
            , markerDeprecated = deprecated
            }
    result <- tryAny (writeFileWithParents path (toStrict (Aeson.encode marker)))
    case result of
      Right () -> return ()
      Left err ->
        $logError
          ( "Failed to write deprecation marker for "
            <> runPackageName pkgName <> ": " <> tshow err
          )

deprecationMarkerFileFor :: PackageName -> Handler FilePath
deprecationMarkerFileFor pkgName = do
  dir <- getDataDir
  return $
    dir </> "cache" </> "packages" </>
    unpack (runPackageName pkgName) </> "deprecation-marker.json"

packageNotFound :: PackageName -> Handler a
packageNotFound pkgName = do
  defaultLayout404 $(widgetFile "packageNotFound")

packageVersionNotFound :: PackageName -> Version -> Handler a
packageVersionNotFound pkgName version = do
  availableVersions <- map sort $ availableVersionsFor pkgName
  defaultLayout404 $(widgetFile "packageVersionNotFound")

defaultLayout404 :: Widget -> Handler a
defaultLayout404 widget =
  defaultLayout widget >>= sendResponseStatus notFound404

versionSelector :: PackageName -> Version -> WidgetFor App ()
versionSelector pkgName version = do
  let route = PackageAvailableVersionsR (PathPackageName pkgName)
  availableVersionsUrl <- getUrlRender <*> pure route
  $(widgetFile "versionSelector")

-- | Try to parse a D.UploadedPackage from a JSON Value.
parseUploadedPackage ::
  Value ->
  Handler (Either (ABE.ParseError D.PackageError) D.UploadedPackage)
parseUploadedPackage value = do
  minVersion <- appMinimumCompilerVersion . appSettings <$> getYesod
  return $ ABE.parseValue (D.asUploadedPackage minVersion) value

displayJsonError :: Value -> ABE.ParseError D.PackageError -> [Text]
displayJsonError value e = case e of
  ABE.InvalidJSON _ ->
    ["The file you submitted was not valid JSON."]
  ABE.BadSchema _ _ ->
    ABE.displayError D.displayPackageError e ++ extraInfo

  where
  -- Attempt to extract the compiler version that a JSON upload was created
  -- with.
  extractVersion =
    toObject
    >=> KM.lookup "compilerVersion"
    >=> toString
    >=> (parseVersion' . unpack)

  toObject json =
    case json of
      Aeson.Object obj -> Just obj
      _ -> Nothing

  toString json =
    case json of
      Aeson.String str -> Just str
      _ -> Nothing

  -- Some extra information about what might have caused an error.
  extraInfo =
    case extractVersion value of
      Just v | v > P.version ->
        let pursuitVersion = pack (showVersion P.version) in
        [ "Usually, this occurs because the JSON data was generated with a newer " <>
          "version of the compiler than what Pursuit is currently using, and " <>
          "the JSON format changed between compiler releases."
        , "This data was generated with " <> pack (showVersion v) <> " of the compiler."
        , "Pursuit is currently using " <> pursuitVersion <> "."
        , "You might be able to fix this by temporarily downgrading to " <>
           pursuitVersion <> " to generate the JSON data."
        ]
      _ ->
        []

versionSeries :: Version -> String
versionSeries (Version (0:x:_) _) = "0." ++ show x ++ ".x"
versionSeries (Version (x:_) _) = show x ++ ".x"
versionSeries _ = "*"
