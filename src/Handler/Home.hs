module Handler.Home where

import Import
import qualified Templates

getHomeR :: Handler RenderedHtml
getHomeR =
  lucid Templates.home
