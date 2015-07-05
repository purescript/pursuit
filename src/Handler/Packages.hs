module Handler.Packages where

import Import
import Text.Blaze.Html (preEscapedToHtml)
import Text.Julius (rawJS)
import Data.Version
import qualified Language.PureScript.Docs as D
import Web.Bower.PackageMeta (PackageName, runPackageName, bowerDependencies, bowerLicence)
import qualified Data.Aeson.BetterErrors as A

import Handler.Database
import Handler.Caching
import Handler.GithubOAuth
import TemplateHelpers

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
    return $ toJSON $ map toPair vs
  where
  alternateVersionUrl v = PackageVersionR (PathPackageName pkgName) (PathVersion v)

getPackageVersionR :: PathPackageName -> PathVersion -> Handler Html
getPackageVersionR (PathPackageName pkgName) (PathVersion version) =
  cacheHtml $ findPackage pkgName version $ \pkg@D.Package{..} -> do
    moduleList <- renderModuleList pkg
    mreadme    <- tryGetReadme pkg
    defaultLayout $ do
      setTitle (toHtml (runPackageName pkgName))
      let dependencies = bowerDependencies pkgMeta
      $(widgetFile "packageVersion")

getPackageIndexR :: Handler Html
getPackageIndexR = redirect HomeR

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
    Just pkg -> cont pkg
    Nothing -> packageNotFound pkgName

packageNotFound :: PackageName -> Handler a
packageNotFound pkgName = do
  content <- defaultLayout $(widgetFile "packageNotFound")
  sendResponseStatus notFound404 content

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
      <div .col-main>
        <h1>
          package
          <a href=@{packageRoute pkg}>#{runPackageName pkgName}

      ^{versionSelector pkgName pkgVersion}

    <div .col-main>
      ^{widget}
    |]

uploadPackageForm :: Html -> MForm Handler (FormResult FileInfo, Widget)
uploadPackageForm = renderDivs $ areq fileField "" Nothing

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
        case D.parseUploadedPackage bytes of
          Right pkg -> do
            let pkg' = D.verifyPackage user pkg
            insertPackage pkg'
            redirect (packageRoute pkg')
          Left err -> renderUploadPackageForm widget enctype
                        (Just $ displayJsonError err)
      _ -> renderUploadPackageForm widget enctype Nothing

displayJsonError :: A.ParseError D.PackageError -> [Text]
displayJsonError e = case e of
  A.InvalidJSON _ -> ["The file you submitted was not valid JSON."]
  _ -> A.displayError D.displayPackageError e
