module Handler.Packages where

import Import
import Text.Blaze.Html (preEscapedToHtml)
import Text.Julius (rawJS)
import Data.Version
import qualified Data.ByteString.Lazy as BL
import qualified Language.PureScript.Docs as D
import Web.Bower.PackageMeta (PackageName, runPackageName, bowerDependencies, bowerLicence)
import qualified Data.Aeson.BetterErrors as A

import Handler.Database
import Handler.Caching
import Handler.GithubOAuth
import Handler.Utils
import TemplateHelpers
import qualified GithubAPI

getHomeR :: Handler Html
getHomeR =
  cacheHtml $ do
    pkgNames <- sort <$> getAllPackageNames
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

getPackageIndexR :: Handler Html
getPackageIndexR = redirect HomeR

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
        let display = A.displayError D.displayPackageError
        in sendResponseStatus badRequest400 $ object [ "error" .= display err ]
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
  availableVersions <- availableVersionsFor pkgName
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
    case result of
      FormSuccess file -> do
        bytes <- runResourceT $ fileSource file $$ sinkLazy
        eresult <- parseUploadedPackage bytes
        case eresult of
          Right pkg -> do
            let pkg' = D.verifyPackage user pkg
            insertPackage pkg'
            setCookieMessage "Your package was uploaded successfully."
            redirect (packageRoute pkg')
          Left err -> renderUploadPackageForm widget enctype
                        (Just $ displayJsonError err)
      _ -> renderUploadPackageForm widget enctype Nothing

-- | Try to parse a D.UploadedPackage from a ByteString containing JSON.
parseUploadedPackage ::
  BL.ByteString ->
  Handler (Either (A.ParseError D.PackageError) D.UploadedPackage)
parseUploadedPackage bytes = do
  minVersion <- appMinimumCompilerVersion . appSettings <$> getYesod
  return $ D.parseUploadedPackage minVersion bytes

displayJsonError :: A.ParseError D.PackageError -> [Text]
displayJsonError e = case e of
  A.InvalidJSON _ -> ["The file you submitted was not valid JSON."]
  _ -> A.displayError D.displayPackageError e
