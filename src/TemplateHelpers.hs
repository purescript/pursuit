
module TemplateHelpers where

import Import hiding (span)
import Text.Blaze.Html5 as H hiding (map, link)
import Text.Blaze.Html5.Attributes as A hiding (span)
import qualified Web.Bower.PackageMeta as Bower
import qualified Language.PureScript as P
import qualified Language.PureScript.Docs as D
import qualified Hoogle

import Model.DocsAsHtml (packageAsHtml, htmlModules)
import Model.DocLinks
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

joinLicenses :: [String] -> Maybe Html
joinLicenses ls
  | null ls   = Nothing
  | otherwise = Just (strong (toHtml (intercalate "/" ls)) >> " licensed")

renderVersionRange :: Bower.VersionRange -> Html
renderVersionRange = toHtml . Bower.runVersionRange

renderModuleList :: D.VerifiedPackage -> Handler Html
renderModuleList pkg = do
  docLinkRenderer <- getDocLinkRenderer
  let docsOutput = packageAsHtml docLinkRenderer pkg
      moduleNames = sort $ map (P.runModuleName . fst) $ htmlModules docsOutput

  withUrlRenderer [hamlet|
    <ul .documentation-contents>
      $forall name <- moduleNames
        <li>
          <a href=@{moduleDocsRoute pkg name}>#{name}
    |]

tryGetReadme :: D.VerifiedPackage -> Handler (Maybe Html)
tryGetReadme D.Package{..} = do
  mtoken <- appGithubAuthToken . appSettings <$> getYesod
  let (ghUser, ghRepo) = pkgGithub
  let ghTag = pkgVersionTag
  ereadme <- GithubAPI.getReadme mtoken ghUser ghRepo ghTag
  case ereadme of
    Right readme ->
      return (Just readme)
    Left err -> do
      $logError (tshow err)
      return Nothing

renderHtmlDocs :: D.VerifiedPackage -> String -> Handler (Maybe Html)
renderHtmlDocs pkg mnString = do
  docLinkRenderer <- getDocLinkRenderer
  let docsOutput = packageAsHtml docLinkRenderer pkg
      mn = P.moduleNameFromString mnString
  return $ map preEscapedToHtml $ lookup mn (htmlModules docsOutput)

-- | Produce a Route for a given DocLink. Note that we do not include the
-- fragment; this is the responsibility of the DocsAsHtml module.
docLinkRoute :: LinksContext' -> DocLink -> Route App
docLinkRoute (LinksContext{..}, srcModule) link = case link of
  SameModule _ ->
    mkRoute ctxPackageName ctxVersion srcModule
  LocalModule _ otherModule _ ->
    mkRoute ctxPackageName ctxVersion otherModule
  DepsModule _ otherPackageName otherVersion otherModule _ ->
    mkRoute otherPackageName otherVersion otherModule
  where
  mkRoute pkgName version modName =
    PackageVersionModuleDocsR
      (PathPackageName pkgName)
      (PathVersion version)
      (P.runModuleName modName)

getDocLinkRenderer :: Handler (LinksContext' -> DocLink -> Text)
getDocLinkRenderer = do
  renderUrl <- getUrlRender
  return $ \ctx link -> renderUrl (docLinkRoute ctx link)

tagStrToHtml :: Hoogle.TagStr -> Html
tagStrToHtml tagStr = case tagStr of
  Hoogle.Str str -> text (pack str)
  Hoogle.Tags tags -> foldMap tagStrToHtml tags
  Hoogle.TagBold tag -> span ! class_ "highlight" $ tagStrToHtml tag
  Hoogle.TagEmph tag -> strong (tagStrToHtml tag)
  Hoogle.TagLink _ tag -> tagStrToHtml tag -- Ignore urls
  Hoogle.TagColor _ tag -> tagStrToHtml tag -- Ignore colours for now

-- | Render a URL together with a fragment (possibly).
getFragmentRender :: Handler ((Route App, Maybe Text) -> Text)
getFragmentRender = do
  render <- getUrlRender
  return $ \(route, fragment) -> render route ++ maybe "" ("#" ++) fragment
