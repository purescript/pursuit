
module Handler.Guides where

import Import

getGuidePackageUploadR :: Handler Html
getGuidePackageUploadR =
  defaultLayout $(widgetFile "guides/packageUpload")
