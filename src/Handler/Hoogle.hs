
module Handler.Hoogle where

import Import
import qualified Data.Text.Lazy as LT
import qualified Hoogle

import Model.DocsAsHoogle (packageAsHoogle)
import Handler.Packages (findPackage)
import Handler.Caching (cacheText)

getPackageHoogleR :: PathPackageName -> PathVersion -> Handler LT.Text
getPackageHoogleR (PathPackageName pkgName) (PathVersion version) = 
  cacheText $ findPackage pkgName version (return . packageAsHoogle)

generateDatabase :: Handler Hoogle.Database
generateDatabase = undefined
