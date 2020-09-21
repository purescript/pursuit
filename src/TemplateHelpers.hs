module TemplateHelpers where

import Import hiding (span, link)
import qualified Data.List.Split as List
import qualified Data.Map as Map
import Data.Traversable (for)
import Data.Bifunctor (first)
import Data.Version (Version)
import Data.List (nub)
import Data.Text (splitOn)
import Data.Time.Format as TimeFormat
import Text.Blaze.Html5 as H hiding (map, link)
import Text.Blaze.Html5.Attributes as A hiding (span, name, start, for)
import qualified Web.Bower.PackageMeta as Bower
import qualified Language.PureScript as P
import qualified Language.PureScript.Docs as D
import Language.PureScript.Docs.AsHtml
import Handler.Database (lookupPackage)

import GithubAPI (ReadmeMissing(..))
import qualified GithubAPI

linkToGithubUser :: D.GithubUser -> Html
linkToGithubUser user =
  a ! href (toValue ("https://github.com/" <> D.runGithubUser user)) $ do
    toHtml (D.runGithubUser user)

linkToGithub :: (D.GithubUser, D.GithubRepo) -> Html
linkToGithub (user, repo) =
  let linkContent = D.runGithubUser user <> "/" <> D.runGithubRepo repo
  in linkToGithub' linkContent (user, repo)

linkToGithub' :: Text -> (D.GithubUser, D.GithubRepo) -> Html
linkToGithub' linkContent (user, repo) =
  let path = D.runGithubUser user <> "/" <> D.runGithubRepo repo
  in a ! href (toValue ("https://github.com/" <> path)) $ do
    toHtml linkContent

joinLicenses :: [Text] -> Maybe Html
joinLicenses ls
  | null ls   = Nothing
  | otherwise = Just (toHtml (intercalate "/" ls))

renderVersionRange :: Bower.VersionRange -> Html
renderVersionRange = toHtml . Bower.runVersionRange

linkToModule :: D.VerifiedPackage -> D.InPackage P.ModuleName -> Handler Html
linkToModule pkg mn' = do
  let mtargetPkg = findTargetPackage pkg mn'
  let mn = D.ignorePackage mn'
  let linkText = insertBreaks mn
  case mtargetPkg of
    Just (pkgName, pkgVersion) ->
      let route = PackageVersionModuleDocsR
                    (PathPackageName pkgName)
                    (PathVersion pkgVersion)
                    (P.runModuleName mn)
      in
        withUrlRenderer [hamlet|<a href=@{route}>#{linkText}|]
    Nothing ->
      withUrlRenderer [hamlet|<span class="link-target-missing">#{linkText}|]

-- | Given a package name and a verified package, attempt to find details of
-- package which defines that module. This may be the package given in the
-- argument or one of its dependencies.
findTargetPackage :: D.VerifiedPackage -> D.InPackage P.ModuleName -> Maybe (Bower.PackageName, Version)
findTargetPackage pkg mn' =
  case mn' of
    D.Local _ ->
      Just (D.packageName pkg, D.pkgVersion pkg)
    D.FromDep pkgName _ ->
      (pkgName,) <$> lookup pkgName (D.pkgResolvedDependencies pkg)

renderModuleList :: D.VerifiedPackage -> Handler Html
renderModuleList pkg = do
  htmlRenderContext <- getHtmlRenderContext
  let docsOutput = packageAsHtml (Just . htmlRenderContext pkg . D.ignorePackage) pkg
      moduleNames = sort . map fst $ htmlModules docsOutput
  moduleLinks <- traverse (linkToModule pkg . D.Local) moduleNames

  moduleListFromLinks moduleLinks

builtinModuleList :: Handler Html
builtinModuleList =
  traverse builtinModuleLink D.primModules >>= moduleListFromLinks
  where
  builtinModuleLink m =
    let
      mn = D.modName m
      route = BuiltinDocsR (P.runModuleName mn)
      linkText = insertBreaks mn
    in
      withUrlRenderer [hamlet|<a href=@{route}>#{linkText}|]

moduleListFromLinks :: [Html] -> Handler Html
moduleListFromLinks moduleLinks =
  withUrlRenderer [hamlet|
    <dl .grouped-list>
      <dt .grouped-list__title>Modules
      $forall link <- moduleLinks
        <dd .grouped-list__item>#{link}
    |]

-- | Insert <wbr> elements in between elements of a module name, in order to
-- prevent awkward line breaks or overflowing (and generating horizontal
-- scrollbars).
insertBreaks :: P.ModuleName -> Html
insertBreaks =
  P.runModuleName
   >>> splitOn "."
   >>> map toHtml
   >>> intercalate (toHtml ("." :: Text) *> wbr)

tryGetReadme :: D.VerifiedPackage -> Handler (Either (ReadmeMissing, D.VerifiedPackage) Html)
tryGetReadme pkg@D.Package{..} = do
  mtoken <- appGithubAuthToken . appSettings <$> getYesod
  let (ghUser, ghRepo) = pkgGithub
  let ghTag = pkgVersionTag
  res <- GithubAPI.getReadme mtoken ghUser ghRepo ghTag
  case res of
    Left (OtherReason r) -> do
      $logError (tshow r)
    _ ->
      return ()
  return $ first (, pkg) res

renderReadme :: Either (ReadmeMissing, D.VerifiedPackage) Html -> Html
renderReadme = \case
  Right html' ->
    html'
  Left (APIRateLimited, D.Package{..}) ->
    let githubLink = linkToGithub' "Github" pkgGithub
    in [shamlet|
          <div .message .message--not-available>
            No readme available (due to rate limiting). Please try again later
            or view this package's readme on ^{githubLink}.
       |]
  Left (ReadmeNotFound, _) ->
    [shamlet|
      <div .message .message--not-available>
        No readme found in the repository at this tag. If you are the maintainer,
        perhaps consider adding one in the next release.
    |]
  Left (OtherReason _, _) ->
    [shamlet|
      <div .message .message--not-available>
        No readme available, for some unexpected reason (which has been logged).
        Perhaps
        <a href="https://github.com/purescript/pursuit/issues/new">
          open an issue?
    |]


renderHtmlDocs :: D.VerifiedPackage -> Text -> Handler (Maybe Html)
renderHtmlDocs pkg mnString = do
  htmlRenderContext <- getHtmlRenderContext
  depHtmlRenderContext <- getDepHtmlRenderContexts pkg
  let docsOutput = flip packageAsHtml pkg $ \case
        D.Local mn -> Just $ htmlRenderContext pkg mn
        D.FromDep pkgName mn -> depHtmlRenderContext pkgName mn
  traverse render $
    lookup (P.moduleNameFromString mnString)
           (htmlModules docsOutput)

  where
  render :: HtmlOutputModule Html -> Handler Html
  render HtmlOutputModule{..} = do
    reexports <- traverse renderReExports htmlOutputModuleReExports
    return (htmlOutputModuleLocals *> mconcat reexports)

  renderReExports :: (D.InPackage P.ModuleName, Html) -> Handler Html
  renderReExports (mn, decls) = do
    moduleLink <- linkToModule pkg mn
    pure $ do
      h2 ! class_ "re-exports" $ do
        text "Re-exports from "
        strong moduleLink
      decls

primDocsFor :: P.ModuleName -> Maybe Html
primDocsFor mn =
  fmap intoHtml $ find ((== mn) . D.modName) D.primModules
  where
  intoHtml m =
    htmlOutputModuleLocals $
      snd $
        moduleAsHtml
          (const (Just nullRenderContext))
          m

-- | Produce a Route for a given DocLink.
docLinkRoute :: D.LinksContext -> D.DocLink -> Route App
docLinkRoute D.LinksContext{..} link = case D.linkLocation link of
  D.LocalModule modName ->
    mkRoute ctxPackageName ctxVersion modName
  D.DepsModule pkgName version modName ->
    mkRoute pkgName version modName
  D.BuiltinModule modName ->
    BuiltinDocsR (P.runModuleName modName)
  where
  mkRoute pkgName version modName =
    PackageVersionModuleDocsR
      (PathPackageName pkgName)
      (PathVersion version)
      (P.runModuleName modName)

getDepHtmlRenderContexts
    :: D.Package a
    -> Handler (Bower.PackageName -> P.ModuleName -> Maybe HtmlRenderContext)
getDepHtmlRenderContexts D.Package {..} = do
  htmlRenderContext <- getHtmlRenderContext
  let reExportedPackages =
        nub $ concat [
          catMaybes [
            case inPkg of
              D.Local _ -> Nothing
              D.FromDep pN _ -> find ((== pN) . fst) pkgResolvedDependencies
          | (inPkg, _) <- modReExports
          ]
        | D.Module {..} <- pkgModules ]

  m <- Map.fromList . catMaybes <$> do
    for reExportedPackages $ \(pkgName, version) -> do
      fmap (pkgName, ) . hush . fmap htmlRenderContext <$> do
        lookupPackage pkgName version

  return $ \pkgName mN -> fmap ($ mN) $ Map.lookup pkgName m

getHtmlRenderContext :: Handler (D.Package a -> P.ModuleName -> HtmlRenderContext)
getHtmlRenderContext = do
  renderUrl <- getUrlRender
  return $ \pkg currentMn ->
    let
      linksContext = D.getLinksContext pkg
    in
      HtmlRenderContext
        { buildDocLink = D.getLink linksContext currentMn
        , renderDocLink = renderUrl . docLinkRoute linksContext
        , renderSourceLink = Just . renderSourceLink' linksContext
        }

renderSourceLink' :: D.LinksContext -> P.SourceSpan -> Text
renderSourceLink' D.LinksContext{..} (P.SourceSpan name start end) =
  concat
    [ githubBaseUrl
    , "/blob/"
    , ctxVersionTag
    , "/"
    , pack (relativeToBase (unpack name))
    , "#", fragment
    ]
  where
  (P.SourcePos startLine _) = start
  (P.SourcePos endLine _) = end
  (D.GithubUser user, D.GithubRepo repo) = ctxGithub

  relativeToBase = intercalate "/" . dropWhile (/= "src") . splitOnPathSep
  githubBaseUrl = concat ["https://github.com/", user, "/", repo]
  fragment = "L" <> tshow startLine <> "-L" <> tshow endLine

-- | Split a string on either unix-style "/" or Windows-style "\\" path
-- | separators.
splitOnPathSep :: String -> [String]
splitOnPathSep str
  | '/'  `elem` str = List.splitOn "/" str
  | '\\' `elem` str = List.splitOn "\\" str
  | otherwise       = [str]

-- | Render a URL together with a fragment (possibly).
getFragmentRender :: Handler ((Route App, Maybe Text) -> Text)
getFragmentRender = do
  render <- getUrlRender
  return $ \(route, fragment) -> render route ++ maybe "" ("#" ++) fragment

formatDate :: UTCTime -> String
formatDate =
  TimeFormat.formatTime
    TimeFormat.defaultTimeLocale
    (TimeFormat.iso8601DateFormat Nothing)
