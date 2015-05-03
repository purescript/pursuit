{-# LANGUAGE RecordWildCards #-}

module Handler.Packages where

import Import
import Web.Bower.PackageMeta (runPackageName)
import Pursuit.Database
import Data.Version
import Language.PureScript.Docs as D

type One = Succ Zero

packageRoute :: D.UploadedPackage -> Route App
packageRoute pkg =
  PackageVersionR (PathPackageName (D.packageName pkg))
                  (PathVersion (D.pkgVersion pkg))

getPackageR :: PathPackageName -> Handler Html
getPackageR ppkgName@(PathPackageName pkgName) = do
  versions <- queryDb (availableVersionsFor pkgName)
  case versions of
    Nothing -> notFound
    Just vs ->
      case toMinLen vs :: Maybe (MinLen One [Version]) of
        Nothing -> notFound
        Just vs' -> redirect (PackageVersionR ppkgName (maximum ((map . map) PathVersion vs')))

getPackageVersionR :: PathPackageName -> PathVersion -> Handler Html
getPackageVersionR (PathPackageName pkgName') (PathVersion version) = do
  pkg' <- queryDb (lookupPackage pkgName' version)
  case pkg' of
    Nothing -> notFound
    Just D.UploadedPackage{..} ->
      defaultLayout $ do
        let pkgName = runPackageName pkgName'
        setTitle (toHtml (pkgName ++ " · Pursuit"))
        $(widgetFile "packageVersion")

getPackageIndexR :: Handler Html
getPackageIndexR = redirect HomeR

postPackageIndexR :: Handler Html
postPackageIndexR = do
  pkg <- requireJsonBody
  updateDb (insertPackage pkg)
  sendResponseCreated (packageRoute pkg)
