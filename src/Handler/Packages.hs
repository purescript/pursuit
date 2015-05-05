module Handler.Packages where

import Import
import Data.Version
import qualified Language.PureScript.Docs as D
import qualified Web.Bower.PackageMeta as Bower

import Model.Database
import TemplateHelpers

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
  findPackage pkgName version $ \pkg@D.Package{..} ->
    defaultLayout $ do
      setTitle (toHtml (Bower.runPackageName pkgName))
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
  findPackage pkgName version $ \pkg@D.Package{..} ->
    defaultLayout $ do
      setTitle (toHtml (Bower.runPackageName pkgName))
      $(widgetFile "packageVersionDocs")

findPackage ::
  Bower.PackageName ->
  Version ->
  (D.VerifiedPackage -> Handler Html) ->
  Handler Html
findPackage pkgName' version cont = do
  pkg' <- queryDb (lookupPackage pkgName' version)
  case pkg' of
    Nothing -> notFound
    Just pkg -> cont pkg

versionSelector :: Version -> WidgetT App IO ()
versionSelector version = $(widgetFile "versionSelector")
