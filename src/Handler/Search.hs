
module Handler.Search
  ( getSearchR
  , SearchResult(..)
  , interleave
  ) where

import Import
import qualified Data.Text as T
import Data.Version (showVersion)
import qualified Web.Bower.PackageMeta as Bower

import Language.PureScript.Docs.AsHtml (makeFragment, renderMarkdown)
import TemplateHelpers (getFragmentRender)

import qualified Text.Blaze as Blaze
import qualified Text.Blaze.Html5 as Html5
import qualified Text.Blaze.Renderer.Text as BlazeT

import qualified Language.PureScript as P

import qualified XMLArrows
import SearchIndex
  (SearchResult(..), SearchResultSource(..), SearchResultInfo(..), SearchIndex,
   searchForName, searchForType)

resultsPerPage :: Int
resultsPerPage = 50

maxPages :: Int
maxPages = 5

queryParam :: Text
queryParam = "q"

pagesParam :: Text
pagesParam = "pages"

partialParam :: Text
partialParam = "partial"

getSearchR :: Handler TypedContent
getSearchR = do
  query <- getQuery
  npages <- getPages
  let limit = npages * resultsPerPage

  (results, hasMore) <- do
    resultSets <- traverse ($ query) searchSources
    let interleavedResults = foldl' interleave [] resultSets
    return . take' limit . map fst $ interleavedResults

  let justThisPage = drop (resultsPerPage * (npages - 1)) results

  urls <- getRelatedUrls query npages hasMore
  addLinkHeader urls

  partial <- lookupGetParam partialParam
  if isJust partial
    then do
      -- XHR; return just the new results and nothing else in an HTML document
      fr <- getFragmentRender

      -- An XHR request includes a special header to let the client know
      -- whether to offer to render more results. If an X-Load-More header is
      -- present, then the value will be a URL which will return more results
      -- as a 'partial' (i.e. a minimal HTML document containing the results
      -- and nothing else). Otherwise, there will be an X-No-More header, whose
      -- value will either be "limited" (if there are further results but
      -- Pursuit has opted not to show them), or "exhausted" (if there are no
      -- further results).
      if hasMore
        then do
          case relatedUrlsPartial urls of
            Just moreUrl ->
              addHeader "X-Load-More" moreUrl
            Nothing ->
              addHeader "X-No-More" "limited"
        else
          addHeader "X-No-More" "exhausted"

      sendResponseStatus ok200 [shamlet|
        <div #results-container>
          $forall r <- justThisPage
            ^{searchResultHtml fr r}
        |]
    else do
      selectRep $ do
        provideRep (htmlOutput query urls results)
        provideRep (jsonOutput justThisPage)

  where
    getQuery :: Handler Text
    getQuery = do
      mquery <- lookupGetParam "q"
      case mquery of
        Nothing ->
          redirect HomeR
        Just query ->
          return (T.strip query)

    getPages :: Handler Int
    getPages = fmap go (lookupGetParam "pages")
      where
      go param =
        case param >>= (readMay . unpack) of
          Just npages -> max 1 (min npages maxPages)
          Nothing -> 1

    htmlOutput :: Text -> RelatedUrls -> [SearchResult] -> Handler Html
    htmlOutput query urls results = do
      fr <- getFragmentRender
      content <- defaultLayout $(widgetFile "search")
      sendResponseStatus ok200 content

    jsonOutput = fmap toJSON . traverse searchResultToJSON

-- Used for rendering URLs in Link headers and in the HTML.
data RelatedUrls = RelatedUrls
  { relatedUrlsNext :: Maybe Text
  , relatedUrlsPrevious :: Maybe Text
  , relatedUrlsPartial :: Maybe Text
  }

getRelatedUrls :: Text -> Int -> Bool -> Handler RelatedUrls
getRelatedUrls query npages hasMore = do
  urlWithParams <- renderSearchUrlParams
  let baseParams = [(queryParam, query)]
  let nextParams =
        if hasMore && npages < maxPages
          then
            Just ((pagesParam, tshow (npages + 1)) : baseParams)
          else
            Nothing

  let nextUrl =
        fmap urlWithParams nextParams
  let partialUrl =
        fmap (\params -> urlWithParams ((partialParam, "true") : params))
             nextParams

  let prevParams =
        if npages > 1
          then
            Just ((pagesParam, tshow (npages - 1)) : baseParams)
          else
            Nothing
  let prevUrl = fmap urlWithParams prevParams

  return RelatedUrls
    { relatedUrlsNext = nextUrl
    , relatedUrlsPrevious = prevUrl
    , relatedUrlsPartial = partialUrl
    }

renderLinkHeader :: RelatedUrls -> Text
renderLinkHeader urls =
  let
    nextLink =
      fmap (\url ->
        [ "<" <> url <> ">"
        , "rel=\"next\""
        , "title=\"Next " <> tshow resultsPerPage <> " results\""
        ]) (relatedUrlsNext urls)
    prevLink =
      fmap (\url ->
        [ "<" <> url <> ">"
        , "rel=\"previous\""
        , "title=\"Previous " <> tshow resultsPerPage <> " results\""
        ]) (relatedUrlsPrevious urls)
  in
    intercalate ", " (map (intercalate "; ") (catMaybes [nextLink, prevLink]))

addLinkHeader :: RelatedUrls -> Handler ()
addLinkHeader urls =
  addHeader "Link" (renderLinkHeader urls)

-- Render a link to the SearchR route with the given parameters.
renderSearchUrlParams :: Handler ([(Text, Text)] -> Text)
renderSearchUrlParams = do
  render <- getUrlRenderParams
  return (render SearchR)

searchResultToJSON :: SearchResult -> Handler Value
searchResultToJSON result@SearchResult{..} = do
  url <- getFragmentRender <*> pure (routeResult result)
  let html = renderMarkdown srComments
  return $
    object [ "package" .= pkg
           , "version" .= showVersion version
           , "markup" .= BlazeT.renderMarkup html
           , "text" .= BlazeT.renderMarkup (Blaze.contents html)
           , "info" .= toJSON srInfo
           , "url" .= url
           ]
  where
  (pkg, version) =
    case srSource of
      SourceBuiltin ->
        ("<builtin>", P.version)
      SourcePackage pn v ->
        (Bower.runPackageName pn, v)

routeResult :: SearchResult -> (Route App, Maybe Text)
routeResult SearchResult{..} =
  case srInfo of
    PackageResult ->
      ( case srSource of
          SourcePackage pkgName _ ->
            PackageR (PathPackageName pkgName)
          SourceBuiltin ->
            -- this shouldn't happen
            HomeR
      , Nothing
      )
    ModuleResult modName ->
      ( moduleRoute modName
      , Nothing
      )
    DeclarationResult ns modName declTitle _ ->
      ( moduleRoute modName
      , Just $ drop 1 $ makeFragment ns declTitle
      )
  where
  moduleRoute =
    case srSource of
      SourceBuiltin ->
        BuiltinDocsR
      SourcePackage pkgName version ->
        PackageVersionModuleDocsR
          (PathPackageName pkgName)
          (PathVersion version)

-- | Like Prelude.take, except also returns a Bool indicating whether the
-- original list has any additional elements after the returned prefix.
take' :: Int -> [a] -> ([a], Bool)
take' n xs =
  let (prefix, rest) = splitAt n xs
   in (prefix, not (null rest))

-- | Interleave two lists. If the arguments are in ascending order (according
-- to their second elements) then the result is also in ascending order.
interleave :: Ord score => [(a,score)] -> [(a,score)] -> [(a,score)]
interleave [] ys = ys
interleave xs [] = xs
interleave (x@(_, scoreX):xs) (y@(_, scoreY):ys) =
  if scoreX > scoreY
    then
      y : interleave (x:xs) ys
    else
      x : interleave xs (y:ys)

searchSources :: [Text -> Handler [(SearchResult, Int)]]
searchSources =
  map conv
    [ searchForName
    , searchForType
    ]
  where
  conv :: (Text -> SearchIndex -> [(SearchResult, Int)]) ->
          Text ->
          Handler [(SearchResult, Int)]
  conv f query = do
    idx <- liftIO . readTVarIO . appSearchIndex =<< getYesod
    return $ f query idx

renderMarkdownNoLinks :: Text -> Html
renderMarkdownNoLinks =
  renderMarkdown
  -- Wrapping in a div is necessary because of how XML arrows work
  >>> Html5.div
  >>> XMLArrows.run XMLArrows.replaceLinks

searchResultHtml :: ((Route App, Maybe Text) -> Text) -> SearchResult -> Html
searchResultHtml fr r =
  [shamlet|
    <div .result>
      <h3 .result__title>
        $case srInfo r
          $of PackageResult
            <span .result__badge.badge.badge--package title="Package">P
            <a .result__link href=#{fr $ routeResult r}>
              #{pkgName}
          $of ModuleResult moduleName
            <span .badge.badge--module title="Module">M
            <a .result__link href=#{fr $ routeResult r}>
              #{moduleName}
          $of DeclarationResult _ _ name _
            <a .result__link href=#{fr $ routeResult r}>
              #{name}

    <div .result__body>
      $case srInfo r
        $of PackageResult
        $of ModuleResult _
        $of DeclarationResult _ _ name typ
          $maybe typeValue <- typ
            <pre .result__signature><code>#{name} :: #{typeValue}</code></pre>

      #{renderMarkdownNoLinks $ srComments r}

    <div .result__actions>
      $case srInfo r
        $of PackageResult
        $of ModuleResult _
          <span .result__actions__item>
            <span .badge.badge--package title="Package">P
            #{pkgName}
        $of DeclarationResult _ moduleName _ _
          <span .result__actions__item>
            <span .badge.badge--package title="Package">P
            #{pkgName}
          <span .result__actions__item>
            <span .badge.badge--module title="Module">M
            #{moduleName}
  |]
  where
  pkgName =
    case srSource r of
      SourceBuiltin ->
        "<builtin>"
      SourcePackage pn _ ->
        Bower.runPackageName pn
