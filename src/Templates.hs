
module Templates where

import Import
import qualified Language.PureScript.Docs as D
import qualified Web.Bower.PackageMeta as Bower

layout :: FromLucid App -> FromLucid App -> FromLucid App
layout title content = do
  url <- lift getUrlRender'
  doctypehtml_ $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [httpEquiv_ "X-UA-Compatible", content_ "IE=edge"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
      title_ title

      styles url
        [ StaticR css_normalize_css
        , StaticR css_style_css
        ]

    body_ $ do
      div_ [class_ "top-banner clearfix"] $ do
        div_ [class_ "container clearfix"] $ do
          div_ [class_ "banner-item pursuit"] $ do
            a_ [class_ "pursuit", href_ (url HomeR)] "Pursuit"

          -- TODO: Real route
          form_ [class_ "banner-item", term "role" "search", action_ "search", method_ "get"] $ do
            label_ [for_ "q", class_ "sr-only"] $
              "Search for packages, types, and functions"
            input_ [type_ "text", class_ "form-control", name_ "q", placeholder_ "Search for packages, types, functions..."]

      div_ [class_ "container", id_ "content"] $ do
        div_ [class_ "clearfix"] $ do
          content

styles :: (a -> Text) -> [a] -> FromLucid App
styles url =
  mapM_ $ \route ->
    link_ [rel_ "stylesheet", type_ "text/css", href_ (url route)]

home :: FromLucid App
home =
  layout "Pursuit" $ do
    h1_ [style_ "text-align: center"] $ do
      strong_ "Pursuit"
      " is the home of PureScript documentation."

    h2_ "Packages:"
    ul_ (li_ (a_ [href_ "/packages/purescript-sequences"] "purescript-sequences"))

packageVersion :: D.VerifiedPackage -> FromLucid App
packageVersion pkg@D.Package{..} =
  let name = Bower.runPackageName (D.packageName pkg)
      title = toHtml (name <> " · Pursuit")
  in layout title $ do
    h1_ (toHtml name)
