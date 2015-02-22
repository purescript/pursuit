{-# LANGUAGE OverloadedStrings #-}

module PursuitServer.HtmlTemplates where

import Lucid
import qualified Data.Text as T

import Web.Scotty (html, ActionM)

stylesheet :: T.Text -> Html ()
stylesheet url = link_ [href_ url, rel_ "stylesheet", type_ "text/css"]

stylesheets :: [T.Text] -> Html ()
stylesheets = mapM_ stylesheet

index :: Html ()
index = doctypehtml_ $ do
  head_ $ do
    title_ "Pursuit"

    meta_ [name_ "viewport", content_ "width=device-width,user-scalable=no"]

    stylesheets [ "https://fonts.googleapis.com/css?family=Roboto:400,300,700"
                , "/css/bootstrap.min.css"
                , "/css/style.css"
                ]
  body_ $ do
    div_ [class_ "container-fluid"] $ do
      div_ [class_ "header"] $ do
        h1_ "Pursuit"
        form_ [action_ "/", method_ "get"] $
          input_ [type_ "search", class_ "form-control", placeholder_ "Search",
                  name_ "q"]

      div_ [class_ "body"] $ do
        p_ "Enter a search term above."
        div_ $ do
          a_ [href_ "https://github.com/purescript/pursuit"] "Source"
          " | "
          a_ [href_ "http://purescript.org"] "PureScript"

renderTemplate :: Html () -> ActionM ()
renderTemplate = html . renderText
