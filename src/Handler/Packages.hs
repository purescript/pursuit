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
getHomeR = do
  pkgNames <- sort <$> getAllPackageNames
  defaultLayout $(widgetFile "homepage")

getPackageR :: PathPackageName -> Handler Html
getPackageR ppkgName@(PathPackageName pkgName) = do
  v <- getLatestVersion pkgName
  case v of
    Nothing -> notFound
    Just v' -> redirect (PackageVersionR ppkgName (PathVersion v'))

getLatestVersion :: PackageName -> Handler (Maybe Version)
getLatestVersion pkgName = do
  vs  <- availableVersionsFor pkgName
  let vs' = toMinLen vs :: Maybe (MinLen One [Version])
  return $ map maximum vs'

getPackageVersionR :: PathPackageName -> PathVersion -> Handler Html
getPackageVersionR (PathPackageName pkgName) (PathVersion version) =
  findPackage pkgName version $ \pkg@D.Package{..} -> do
    moduleList <- cache    (pkgName, Just version, "module-list") (renderModuleList pkg)
    mreadme    <- cacheMay (pkgName, Just version, "readme")      (tryGetReadme pkg)
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
  findPackage pkgName version $ \pkg@D.Package{..} -> do
    mhtmlDocs <- cacheMay (pkgName, Just version, "module_" ++ mnString) $
                  renderHtmlDocs pkg mnString
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
    Nothing -> do
      content <- defaultLayout $(widgetFile "packageNotFound")
      sendResponseStatus notFound404 content

versionSelector :: PackageName -> Version -> WidgetT App IO ()
versionSelector pkgName version = do
  versionSelectorIdent <- newIdent

  let dummyVersion = Version [999,999,999] []
  let dummyVersionStr = showVersion dummyVersion
  dummyRoute  <- maybe HomeR (flip substituteVersion dummyVersion) <$> getCurrentRoute
  dummyRoute' <- getUrlRender <*> pure dummyRoute

  html <- handlerToWidget $ cache (pkgName, Nothing, "version-selector") $ do
    versions' <- availableVersionsFor pkgName
    let versions = sortBy (comparing Down) versions'
    let isLatest v = maybe False (== v) (headMay versions)

    let displayVersion v
          | isLatest v = [hamlet|latest (#{showVersion v})|]
          | otherwise  = [hamlet|#{showVersion v}|]

    -- At this stage, rather than putting a selected attribute on the relevant
    -- <option> tag, we instead mark each <option> with an ID, and select the
    -- appropriate one using JS *outside the `cache` block*. This is because
    -- the cached HTML is reused across every version.
    withUrlRenderer [hamlet|
        <div .col-aside>
          <select id=#{versionSelectorIdent} .version-selector>
            $forall v <- versions
              <option id=#{htmlVersionId v} data-version=#{showVersion v}>
                ^{displayVersion v}
      |]

  toWidget html
  toWidgetBody [julius|
      // Set an onchange handler so that selecting a version in the <select>
      // will navigate to the new page
      var selectorId = "#{rawJS versionSelectorIdent}"
      var selector = document.getElementById(selectorId)
      selector.onchange = function() {
        window.location.href = this.value
      };

      // Set the 'selected' attribute on the current version
      var selectedOption = document.getElementById("#{rawJS (htmlVersionId version)}")
      selectedOption.setAttribute('selected', null)

      // Set the 'value' attribute on each <option> to the URL it should
      // point to
      var options = document.querySelectorAll('select#' + selectorId + ' option')
      var len = options.length
      var placeholderUrl = "#{rawJS dummyRoute'}"
      for (var i = 0; i < len; i++) {
        var option = options[i]
        var version = option.getAttribute('data-version')
        if (version != null) {
          option.setAttribute('value',
            placeholderUrl.replace("#{rawJS dummyVersionStr}", version))
        }
      }
    |]
  where
  htmlVersionId :: Version -> Text
  htmlVersionId v = "selector-version-" ++ pack (showVersion v)

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
renderUploadPackageForm widget enctype merror =
  defaultLayout $(widgetFile "uploadPackage")

getUploadPackageR :: Handler Html
getUploadPackageR =
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
            setMessage "Your package was uploaded succesfully."
            redirect (packageRoute pkg')
          Left err -> renderUploadPackageForm widget enctype
                        (Just $ displayJsonError err)
      _ -> renderUploadPackageForm widget enctype Nothing

displayJsonError :: A.ParseError D.PackageError -> [Text]
displayJsonError e = case e of
  A.InvalidJSON _ -> ["The file you submitted was not valid JSON."]
  _ -> A.displayError D.displayPackageError e
