
module Handler.Hoogle where

import Import
import qualified Data.Text.Lazy as LT

import Model.DocsAsHoogle (packageAsHoogle)
import Handler.Packages (findPackage)

getPackageHoogleR :: PathPackageName -> PathVersion -> Handler LT.Text
getPackageHoogleR (PathPackageName pkgName) (PathVersion version) = 
  findPackage pkgName version (return . packageAsHoogle)
