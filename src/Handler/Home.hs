module Handler.Home where

import Import
-- import TemplateHelpers

getHomeR :: Handler Html
getHomeR =
  defaultLayout $(widgetFile "homepage")
