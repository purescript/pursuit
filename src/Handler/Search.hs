
module Handler.Search
  ( getSearchR
  , SearchResult(..)
  ) where

import Import
import Data.Text (strip)
import Data.Trie (elems, submap)
import Data.Version (showVersion)
import qualified Web.Bower.PackageMeta as Bower

import Language.PureScript.Docs.AsHtml (makeFragment, renderMarkdown)
import TemplateHelpers (getFragmentRender)

import qualified Text.Blaze as Blaze
import qualified Text.Blaze.Html5 as Html5
import qualified Text.Blaze.Renderer.Text as BlazeT
import qualified Text.Parsec.Combinator as Parsec

import qualified Language.PureScript as P

import qualified XMLArrows

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

  (results, hasMore) <- case tryParseType query of
    Just ty | not (isSimpleType ty) -> searchForType limit ty
    _ -> searchForName limit (toLower query)

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
          return (strip query)

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

    isSimpleType :: P.Type -> Bool
    isSimpleType P.TypeVar{} = True
    isSimpleType P.TypeConstructor{} = True
    isSimpleType _ = False

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

parseWithTokenParser :: P.TokenParser a -> Text -> Maybe a
parseWithTokenParser p =
  hush . (P.lex "") >=> hush . (P.runTokenParser "" (p <* Parsec.eof))

tryParseType :: Text -> Maybe P.Type
tryParseType = parseWithTokenParser P.parsePolyType

isSymbol :: Text -> Bool
isSymbol = maybe False (const True) . parseWithTokenParser P.symbol

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


-- Like Prelude.take, except also returns a Bool indicating whether the
-- original list has any additional elements after the returned prefix.
take' :: Int -> [a] -> ([a], Bool)
take' n xs =
  let (prefix, rest) = splitAt n xs
   in (prefix, not (null rest))

searchForName :: Int -> Text -> Handler ([SearchResult], Bool)
searchForName limit query = do
  db <- atomically . readTVar =<< (appDatabase <$> getYesod)
  let query' = if isSymbol query then "(" <> query else query
  return (take' limit (map fst (concat (elems (submap (encodeUtf8 query') db)))))

searchForType :: Int -> P.Type -> Handler ([SearchResult], Bool)
searchForType limit ty = do
    db <- atomically . readTVar =<< (appDatabase <$> getYesod)
    return (take' limit (map fst (sortBy (comparing snd) (mapMaybe (matches ty) (concat (elems db))))))
  where
    matches :: P.Type -> (a, Maybe P.Type) -> Maybe (a, Int)
    matches ty1 (a, Just ty2) = do
      score <- compareTypes ty1 ty2
      return (a, score)
    matches _ _ = Nothing

    -- This is an approximation to type subsumption / unification.
    -- This function returns Just a score if there is a possible match,
    -- or Nothing otherwise. Lower scores are better.
    compareTypes :: P.Type -> P.Type -> Maybe Int
    compareTypes (P.TypeVar _) (P.TypeVar _) = Just 0
    compareTypes t (P.TypeVar _) = Just (1 + typeComplexity t)
    compareTypes (P.TypeLevelString s1) (P.TypeLevelString s2) | s1 == s2 = Just 0
    compareTypes (P.TypeWildcard _) t = Just (typeComplexity t)
    compareTypes (P.TypeConstructor q1) (P.TypeConstructor q2) | compareQual q1 q2 = Just 0
    -- There is a special case for functions, since if the user _asked_ for a function,
    -- they probably don't want to see something more general of type 'f a' or 'f a b'.
    compareTypes (P.TypeApp a b) (P.TypeApp c d)
      | not (isFunction a && not (isFunction c)) = (+) <$> compareTypes a c <*> compareTypes b d
    compareTypes (P.ForAll _ t1 _) t2 = compareTypes t1 t2
    compareTypes t1 (P.ForAll _ t2 _) = compareTypes t1 t2
    compareTypes (P.ConstrainedType _ t1) t2 = compareTypes t1 t2
    compareTypes t1 (P.ConstrainedType _ t2) = compareTypes t1 t2
    compareTypes P.REmpty P.REmpty = Just 0
    compareTypes t1@P.RCons{} t2 = compareRows t1 t2
    compareTypes t1 t2@P.RCons{} = compareRows t1 t2
    compareTypes (P.KindedType t1 _) t2 = compareTypes t1 t2
    compareTypes t1 (P.KindedType t2 _) = compareTypes t1 t2
    -- Really, we should desugar any type operators here.
    -- Since type operators are not supported in search right now, this is fine,
    -- since we only care about functions, which are already in the correct
    -- order as they come out of the parser.
    compareTypes (P.ParensInType t1) t2 = compareTypes t1 t2
    compareTypes t1 (P.ParensInType t2) = compareTypes t1 t2
    compareTypes _ _ = Nothing

    isFunction :: P.Type -> Bool
    isFunction (P.TypeConstructor (P.Qualified _ (P.ProperName "Function"))) = True
    isFunction _ = False

    compareRows :: P.Type -> P.Type -> Maybe Int
    compareRows r1 r2 = sum <$>
      sequence [ compareTypes t1 t2
               | (name, t1) <- fst (P.rowToList r1)
               , (name', t2) <- fst (P.rowToList r2)
               , name == name'
               ]

    typeComplexity :: P.Type -> Int
    typeComplexity (P.TypeApp a b) = 1 + typeComplexity a + typeComplexity b
    typeComplexity (P.ForAll _ t _) = 1 + typeComplexity t
    typeComplexity (P.ConstrainedType _ t) = typeComplexity t + 1
    typeComplexity P.REmpty = 0
    typeComplexity (P.RCons _ t r) = 1 + typeComplexity t + typeComplexity r
    typeComplexity (P.KindedType t _) = typeComplexity t
    typeComplexity (P.ParensInType t) = typeComplexity t
    typeComplexity _ = 0

    compareQual :: Eq a => P.Qualified a -> P.Qualified a -> Bool
    compareQual (P.Qualified (Just mn1) a1) (P.Qualified (Just mn2) a2) = mn1 == mn2 && a1 == a2
    compareQual (P.Qualified _ a1) (P.Qualified _ a2) = a1 == a2

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
