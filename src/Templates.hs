module Templates where

import Import
import Data.Version (showVersion)
import qualified Data.Text as T
import qualified Language.PureScript.Docs as D
import qualified Web.Bower.PackageMeta as Bower

import TemplateHelpers

role_ :: Term arg result => arg -> result
role_ = term "role"

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
          form_ [class_ "banner-item", role_ "search", action_ "search", method_ "get"] $ do
            label_ [for_ "q", class_ "sr-only"] $
              "Search for packages, types, and functions"
            input_ [type_ "text", class_ "form-control", name_ "q", placeholder_ "Search for packages, types, functions..."]

      div_ [class_ "container", id_ "content"] $ do
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
    ul_ $ do
      pkg "purescript-sequences"
      pkg "purescript-prelude"
  where
  pkg :: Text -> FromLucid App
  pkg x = li_ (a_ [href_ ("/packages/" <> x)] (toHtml x))

packageVersion :: D.VerifiedPackage -> FromLucid App
packageVersion pkg@D.Package{..} =
  let name = T.pack (Bower.runPackageName (D.packageName pkg))
      title = toHtml (name <> " · Pursuit")
  in layout title $ do
    div_ [class_ "clearfix"] (packageTitle name)
    div_ [class_ "col-aside"] sidebar

  where
  packageTitle :: Text -> FromLucid App
  packageTitle name = do
    div_ [class_ "col-main"] $ do
      h1_ $ do
        "package "
        strong_ (toHtml name)
    div_ [class_ "col-aside version-selector"] $ do
      "latest ("
      strong_ (toHtml (showVersion pkgVersion))
      ") "
      a_ [href_ "#"] "▼"

  sidebar :: FromLucid App
  sidebar =
    intercalate (hr_ []) $ catMaybes $
      [ Just publisher
      , Just github
      , licenses (Bower.bowerLicence pkgMeta)
      , Just documentation
      ]

  publisher = p_ ("published by " >> linkToGithubUser pkgUploader)
  github    = p_ (linkToGithub pkgGithub >> " on github")

  licenses :: [String] -> Maybe (FromLucid App)
  licenses ls
    | null ls   = Nothing
    | otherwise = Just (strong_ (toHtml (intercalate "/" ls)) >> " licensed")

  documentation :: FromLucid App
  documentation = do
    url <- renderUrl (packageDocsRoute pkg)
    a_ [href_ url] "docs"

