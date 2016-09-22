
module Handler.Search
  ( getSearchR
  , SearchResult(..)
  ) where

import Import
import Data.Trie (elems, submap)
import Data.Version (showVersion)
import qualified Web.Bower.PackageMeta as Bower

import Model.DocsAsHtml (makeFragment)
import TemplateHelpers (getFragmentRender)

import Cheapskate (allowRawHtml, markdown)
import qualified Text.Blaze as Blaze
import qualified Text.Blaze.Renderer.Text as Blaze

getSearchR :: Handler TypedContent
getSearchR = do
  mquery <- (map . map) unpack $ lookupGetParam "q"
  case mquery of
    Nothing -> redirect HomeR
    Just query -> do
      results <- searchForName (toLower query)
      selectRep $ do
        provideRep (htmlOutput query results)
        provideRep (jsonOutput results)
  where
    htmlOutput :: String -> [SearchResult] -> Handler Html
    htmlOutput query results = do
      fr <- getFragmentRender
      content <- defaultLayout $(widgetFile "search")
      sendResponseStatus ok200 content

    jsonOutput = fmap toJSON . traverse searchResultToJSON

searchResultToJSON :: SearchResult -> Handler Value
searchResultToJSON result@SearchResult{..} = do
  url <- getFragmentRender <*> pure (routeResult result)
  let html = renderComments hrComments
  return $
    object [ "package" .= hrPkgName
           , "version" .= showVersion hrPkgVersion
           , "markup" .= Blaze.renderMarkup html
           , "text" .= Blaze.renderMarkup (Blaze.contents html)
           , "info" .= toJSON hrInfo
           , "url" .= url
           ]

routeResult :: SearchResult -> ((Route App), Maybe Text)
routeResult SearchResult{..} =
  case hrInfo of
    PackageResult ->
      ( PackageR ppkgName
      , Nothing
      )
    ModuleResult modName ->
      ( PackageVersionModuleDocsR ppkgName pversion modName
      , Nothing
      )
    DeclarationResult typeOrValue modName declTitle typeText ->
      ( PackageVersionModuleDocsR ppkgName pversion modName
      , Just $ pack $ drop 1 $ makeFragment typeOrValue declTitle
      )
  where
  ppkgName = PathPackageName hrPkgName
  pversion = PathVersion hrPkgVersion

searchForName :: String -> Handler [SearchResult]
searchForName query = do
  db <- atomically . readTVar =<< (appDatabase <$> getYesod)
  return (take 50 (concat (elems (submap (fromString query) db))))

renderComments :: String -> Html
renderComments = Blaze.toMarkup . markdown opts . pack
  where
    opts = def { allowRawHtml = False }
