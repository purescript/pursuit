{-# LANGUAGE RecordWildCards #-}

module Handler.Packages where

import Import
import Pursuit.Database
import Data.Version
import Language.PureScript.Docs as D
import qualified Templates

type One = Succ Zero

packageRoute :: D.UploadedPackage -> Route App
packageRoute pkg =
  PackageVersionR (PathPackageName (D.packageName pkg))
                  (PathVersion (D.pkgVersion pkg))

getPackageR :: PathPackageName -> Handler RenderedHtml
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

getPackageVersionR :: PathPackageName -> PathVersion -> Handler RenderedHtml
getPackageVersionR (PathPackageName pkgName') (PathVersion version) = do
  pkg' <- queryDb (lookupPackage pkgName' version)
  case pkg' of
    Nothing -> notFound
    Just pkg ->
      lucid (Templates.packageVersion pkg)

getPackageIndexR :: Handler RenderedHtml
getPackageIndexR = redirect HomeR

postPackageIndexR :: Handler RenderedHtml
postPackageIndexR = do
  pkg <- requireJsonBody
  updateDb (insertPackage pkg)
  sendResponseCreated (packageRoute pkg)
