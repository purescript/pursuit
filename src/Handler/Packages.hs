module Handler.Packages where

import Import
import Text.Blaze.Html (preEscapedToHtml)
import Text.Julius (rawJS)
import Data.Version
import qualified Language.PureScript as P
import qualified Language.PureScript.Docs as D
import qualified Web.Bower.PackageMeta as Bower

import Model.Database
import Model.DocsAsHtml
import TemplateHelpers
import qualified GithubAPI

getPackageR :: PathPackageName -> Handler Html
getPackageR ppkgName@(PathPackageName pkgName) = do
  versions <- queryDb (availableVersionsFor pkgName)
  case versions of
    Nothing -> notFound
    Just vs ->
      case toMinLen vs :: Maybe (MinLen One [Version]) of
        Nothing -> notFound
        Just vs' ->
          let latestVersion = maximum ((map . map) PathVersion vs')
          in redirect (PackageVersionR ppkgName latestVersion)

getPackageVersionR :: PathPackageName -> PathVersion -> Handler Html
getPackageVersionR (PathPackageName pkgName) (PathVersion version) =
  findPackage pkgName version $ \availableVersions pkg@D.Package{..} -> do
    mreadme <- tryGetReadme pkg
    defaultLayout $ do
      setTitle (toHtml (Bower.runPackageName pkgName))
      let dependencies = Bower.bowerDependencies pkgMeta
      $(widgetFile "packageVersion")

getPackageIndexR :: Handler Html
getPackageIndexR = redirect HomeR

postPackageIndexR :: Handler Html
postPackageIndexR = do
  pkg <- requireJsonBody
  -- TODO: Actual verification
  let verifiedPkg = D.verifyPackage (D.GithubUser "hdgarrood") pkg
  updateDb (insertPackage verifiedPkg)
  sendResponseCreated (packageRoute verifiedPkg)

getPackageVersionDocsR :: PathPackageName -> PathVersion -> Handler Html
getPackageVersionDocsR (PathPackageName pkgName) (PathVersion version) =
  findPackage pkgName version $ \availableVersions pkg@D.Package{..} -> do
    let docsOutput = packageAsHtml pkg
    let moduleNames = sort $ map (P.runModuleName . fst) $ htmlModules docsOutput
    defaultLayout $ do
      setTitle (toHtml (Bower.runPackageName pkgName))
      documentationPage availableVersions pkg $
        $(widgetFile "packageVersionDocs")

getPackageVersionModuleDocsR :: PathPackageName -> PathVersion -> String -> Handler Html
getPackageVersionModuleDocsR (PathPackageName pkgName) (PathVersion version) mnString =
  findPackage pkgName version $ \availableVersions pkg@D.Package{..} -> do
    let docsOutput = packageAsHtml pkg
    case lookup (P.moduleNameFromString mnString) (htmlModules docsOutput) of
      Nothing -> notFound
      Just htmlDocs ->
        defaultLayout $ do
          setTitle (toHtml (mnString <> " - " <> Bower.runPackageName pkgName))
          documentationPage availableVersions pkg $
            $(widgetFile "packageVersionModuleDocs")

findPackage ::
  Bower.PackageName ->
  Version ->
  ([Version] -> D.VerifiedPackage -> Handler Html) ->
  Handler Html
findPackage pkgName version cont = do
  pkg' <- queryDb (lookupPackage pkgName version)
  case pkg' of
    Nothing -> notFound
    Just pkg -> do
      versions' <- queryDb (availableVersionsFor pkgName)
      case versions' of
        Nothing -> notFound
        Just versions -> cont versions pkg

versionSelector :: Version -> [Version] -> WidgetT App IO ()
versionSelector version availableVersions' = do
  let availableVersions = sortBy (comparing Down) availableVersions'
  let isLatest v = maybe False (== v) (headMay availableVersions)
  mroute <- getCurrentRoute
  let versionRoute v =
        case mroute of
          Just route -> substituteVersion route v
          Nothing ->    HomeR -- should never happen

  let displayVersion v
        | isLatest v = [whamlet|latest (#{showVersion v})|]
        | otherwise = [whamlet|#{showVersion v}|]

  versionSelectorIdent <- newIdent
  $(widgetFile "versionSelector")

documentationPage ::
  [Version] -> D.VerifiedPackage -> WidgetT App IO () -> WidgetT App IO ()
documentationPage availableVersions pkg@D.Package{..} widget =
  let pkgName = D.packageName pkg
  in [whamlet|
    <div .clearfix>
      <div .col-main>
        <h1>
          <a href=@{packageRoute pkg}>#{Bower.runPackageName pkgName}
          /
          <a href=@{packageDocsRoute pkg}>documentation

      ^{versionSelector pkgVersion availableVersions}

    <div .col-main>
      ^{widget}
    |]

tryGetReadme :: D.VerifiedPackage -> Handler (Maybe Html)
tryGetReadme D.Package{..} = do
  mtoken <- appGithubAuthToken . appSettings <$> getYesod
  let (ghUser, ghRepo) = pkgGithub
  let ghTag = pkgVersionTag
  ereadme <- liftIO (GithubAPI.getReadme mtoken ghUser ghRepo ghTag)
  case ereadme of
    Right readme ->
      return (Just readme)
    Left err -> do
      $logError (tshow err)
      return Nothing
