module TemplateHelpers where

import Import hiding (span)
import qualified Data.List.Split as List
import Data.Version (Version)
import Data.Text (splitOn)
import Text.Blaze.Html5 as H hiding (map, link)
import Text.Blaze.Html5.Attributes as A hiding (span, name, start)
import qualified Web.Bower.PackageMeta as Bower
import qualified Language.PureScript as P
import qualified Language.PureScript.Docs as D
import Language.PureScript.Docs.AsHtml

import GithubAPI (ReadmeMissing(..))
import qualified GithubAPI

linkToGithubUser :: D.GithubUser -> Html
linkToGithubUser user =
  a ! href (toValue ("https://github.com/" <> D.runGithubUser user)) $ do
    toHtml (D.runGithubUser user)

linkToGithub :: (D.GithubUser, D.GithubRepo) -> Html
linkToGithub (user, repo) =
  let path = D.runGithubUser user <> "/" <> D.runGithubRepo repo
  in a ! href (toValue ("https://github.com/" <> path)) $ do
    toHtml path

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
  let docsOutput = packageAsHtml (htmlRenderContext pkg) pkg
      moduleNames = sort . map fst $ htmlModules docsOutput
  moduleLinks <- traverse (linkToModule pkg . D.Local) moduleNames

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

tryGetReadme :: D.VerifiedPackage -> Handler (Either ReadmeMissing Html)
tryGetReadme D.Package{..} = do
  mtoken <- appGithubAuthToken . appSettings <$> getYesod
  let (ghUser, ghRepo) = pkgGithub
  let ghTag = pkgVersionTag
  res <- GithubAPI.getReadme mtoken ghUser ghRepo ghTag
  case res of
    Left (OtherReason r) -> do
      $logError (tshow r)
    _ ->
      return ()
  return res

renderReadme :: Either ReadmeMissing Html -> Html
renderReadme = \case
  Right html' ->
    html'
  Left APIRateLimited ->
    [shamlet|
      <div .message .message--not-available>
        No readme available (due to rate limiting). Please try again later.
    |]
  Left ReadmeNotFound ->
    [shamlet|
      <div .message .message--not-available>
        No readme found in the repository at this tag. If you are the maintainer,
        perhaps consider adding one in the next release.
    |]
  Left (OtherReason _) ->
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
  let docsOutput = packageAsHtml (htmlRenderContext pkg) pkg
      mn = P.moduleNameFromString mnString
  traverse render $ lookup mn (htmlModules docsOutput)

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

primDocs :: Html
primDocs =
  htmlOutputModuleLocals $
    snd $
      moduleAsHtml
        (nullRenderContext (P.moduleNameFromString "Prim"))
        D.primDocsModule

-- | Produce a Route for a given DocLink.
docLinkRoute :: D.LinksContext -> P.ModuleName -> D.DocLink -> Route App
docLinkRoute D.LinksContext{..} srcModule link = case D.linkLocation link of
  D.SameModule ->
    mkRoute ctxPackageName ctxVersion srcModule
  D.LocalModule _ otherModule ->
    mkRoute ctxPackageName ctxVersion otherModule
  D.DepsModule _ otherPackageName otherVersion otherModule ->
    mkRoute otherPackageName otherVersion otherModule
  D.BuiltinModule otherModule ->
    BuiltinDocsR (P.runModuleName otherModule)
  where
  mkRoute pkgName version modName =
    PackageVersionModuleDocsR
      (PathPackageName pkgName)
      (PathVersion version)
      (P.runModuleName modName)

getHtmlRenderContext :: Handler (D.Package a -> P.ModuleName -> HtmlRenderContext)
getHtmlRenderContext = do
  renderUrl <- getUrlRender
  return $ \pkg currentMn ->
    let
      linksContext = D.getLinksContext pkg
    in
      HtmlRenderContext
        { currentModuleName = currentMn
        , buildDocLink = D.getLink linksContext currentMn
        , renderDocLink = renderUrl . docLinkRoute linksContext currentMn
        , renderSourceLink = renderSourceLink' linksContext
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
