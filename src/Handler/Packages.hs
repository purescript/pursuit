module Handler.Packages where

import Import
import Text.Julius (rawJS)
import Text.Blaze (ToMarkup, toMarkup)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Error.Class (throwError)
import qualified Data.Char as Char
import Data.Version
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import qualified Language.PureScript.Docs as D
import Web.Bower.PackageMeta (PackageName, runPackageName, bowerDependencies, bowerLicense)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.BetterErrors as A
import qualified Language.PureScript as P

import Handler.Database
import Handler.Caching
import Handler.GithubOAuth
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
    let firstLetter :: PackageName -> Maybe FirstLetter
        firstLetter = fmap FirstLetter . headMay . stripIntro . runPackageName

        stripIntro :: String -> String
        stripIntro s = fromMaybe s (stripPrefix "purescript-" s)

        pkgNamesByLetter :: [[PackageName]]
        pkgNamesByLetter = groupBy ((==) `on` (firstLetter )) pkgNames
    defaultLayout $(widgetFile "homepage")

getPackageR :: PathPackageName -> Handler Html
getPackageR ppkgName@(PathPackageName pkgName) = do
  v <- getLatestVersionFor pkgName
  case v of
    Nothing -> packageNotFound pkgName
    Just v' -> redirect (PackageVersionR ppkgName (PathVersion v'))

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
    findPackage pkgName version $ \pkg@D.Package{..} -> do
      moduleList <- renderModuleList pkg
      mreadme    <- tryGetReadme pkg
      let cacheStatus = maybe NotOkToCache (const OkToCache) mreadme
      content <- defaultLayout $ do
        setTitle (toHtml (runPackageName pkgName))
        let dependencies = bowerDependencies pkgMeta
        $(widgetFile "packageVersion")
      return (cacheStatus, content)

getPackageIndexR :: Handler TypedContent
getPackageIndexR =
  selectRep $ do
    provideRep (redirect HomeR :: Handler Html)
    provideRep . cacheText . map unlines $ pkgNames
    provideRep . cacheJSON . map toJSON  $ pkgNames
  where
  pkgNames :: Handler [TL.Text]
  pkgNames = map (pack . runPackageName) <$> getAllPackageNames

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
    body     <- getRequestBody
    epackage <- parseUploadedPackage body
    case epackage of
      Left err ->
        let errorMessage = unlines $ displayJsonError body err
        in sendResponseStatus badRequest400 $ object [ "error" .= errorMessage ]
      Right package ->
        return package

  getRequestBody =
    rawRequestBody $$ sinkLazy

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
  findPackage pkgName version $ \pkg@D.Package{..} ->
    redirect (packageRoute pkg)

getPackageVersionModuleDocsR :: PathPackageName -> PathVersion -> String -> Handler Html
getPackageVersionModuleDocsR (PathPackageName pkgName) (PathVersion version) mnString =
  cacheHtml $ findPackage pkgName version $ \pkg@D.Package{..} -> do
    mhtmlDocs <- renderHtmlDocs pkg mnString
    case mhtmlDocs of
      Nothing -> notFound
      Just htmlDocs ->
        defaultLayout $ do
          let mn = P.moduleNameFromString mnString
          setTitle (toHtml (mnString <> " - " <> runPackageName pkgName))
          documentationPage pkg $
            $(widgetFile "packageVersionModuleDocs")

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

versionSelector :: PackageName -> Version -> WidgetT App IO ()
versionSelector pkgName version = do
  versionSelectorIdent <- newIdent
  let route = PackageAvailableVersionsR (PathPackageName pkgName)
  availableVersionsUrl <- getUrlRender <*> pure route
  $(widgetFile "versionSelector")

documentationPage ::
  D.VerifiedPackage -> WidgetT App IO () -> WidgetT App IO ()
documentationPage pkg@D.Package{..} widget =
  let pkgName = D.packageName pkg
  in [whamlet|
    <div .clearfix>
      <div .col.col-main>
        <h1>
          package
          <a href=@{packageRoute pkg}>#{runPackageName pkgName}

      ^{versionSelector pkgName pkgVersion}

    <div .col.col-main>
      ^{widget}
    |]

uploadPackageForm :: Html -> MForm Handler (FormResult FileInfo, Widget)
uploadPackageForm = renderDivs $ areq fileField settings Nothing
  where
  -- This should make the file selection dialog only display json files.
  settings = (fromString "") { fsAttrs = [("accept", "application/json")] }

renderUploadPackageForm :: Widget -> Enctype -> Maybe [Text] -> Handler Html
renderUploadPackageForm widget enctype merror = do
  fr <- getFragmentRender
  defaultLayout $(widgetFile "uploadPackage")

getUploadPackageR :: Handler Html
getUploadPackageR = do
  requireAuthentication $ \_ -> do
    (widget, enctype) <- generateFormPost uploadPackageForm
    renderUploadPackageForm widget enctype Nothing

postUploadPackageR :: Handler Html
postUploadPackageR =
  requireAuthentication $ \user -> do
    ((result, widget), enctype) <- runFormPost uploadPackageForm
    either (renderUploadPackageForm widget enctype) pure =<< handleFormResult user result

  where
  handleFormResult user result = runExceptT $ do
    file <- ExceptT . pure . unpackResult $ result
    bytes <- lift . runResourceT $ fileSource file $$ sinkLazy
    pkg <- ExceptT . fmap (first (Just . displayJsonError bytes)) $ parseUploadedPackage bytes

    when (null (bowerLicense (D.pkgMeta pkg))) $
      throwError (Just ["No license specified. Packages must specify their " ++
                        "license in bower.json."])

    let pkg' = D.verifyPackage user pkg
    lift $ do
      insertPackage pkg'
      setCookieMessage "Your package was uploaded successfully."
      redirect (packageRoute pkg')

  unpackResult r = case r of
    FormSuccess file ->
      Right file
    _ ->
      Left Nothing

-- | Try to parse a D.UploadedPackage from a ByteString containing JSON.
parseUploadedPackage ::
  BL.ByteString ->
  Handler (Either (A.ParseError D.PackageError) D.UploadedPackage)
parseUploadedPackage bytes = do
  minVersion <- appMinimumCompilerVersion . appSettings <$> getYesod
  return $ D.parseUploadedPackage minVersion bytes

displayJsonError :: BL.ByteString -> A.ParseError D.PackageError -> [Text]
displayJsonError bytes e = case e of
  A.InvalidJSON _ ->
    ["The file you submitted was not valid JSON."]
  A.BadSchema _ _ ->
    A.displayError D.displayPackageError e ++ extraInfo

  where
  -- Attempt to extract the compiler version that a JSON upload was created
  -- with.
  extractVersion =
    Aeson.decode
    >=> toObject
    >=> HashMap.lookup "compilerVersion"
    >=> toString
    >=> (D.parseVersion' . unpack)

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
    case extractVersion bytes of
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
