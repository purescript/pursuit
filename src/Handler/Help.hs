
module Handler.Help where

import Import
import Handler.Caching
import EmbeddedDocs

getHelpR :: Handler Html
getHelpR =
  cacheHtml $
    defaultLayout $(widgetFile "help")
