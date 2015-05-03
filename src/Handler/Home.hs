module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
  defaultLayout $ do
    setTitle "Welcome To Yesod!"
    $(widgetFile "homepage")
