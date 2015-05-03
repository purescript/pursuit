
module Handler.Packages where

import Import
import Web.Bower.PackageMeta (runPackageName)

getPackageR :: PathPackageName -> Handler Html
getPackageR (PathPackageName pkgName') =
  let pkgName = runPackageName pkgName'
  in defaultLayout $ do
    setTitle (toHtml ("Package " ++ pkgName))
    $(widgetFile "package")
