
module TemplateHelpers where

import Import
import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes as A
import qualified Web.Bower.PackageMeta as Bower
import qualified Language.PureScript as P
import qualified Language.PureScript.Docs as D

import Model.DocsAsHtml (packageAsHtml, htmlModules)
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
renderModuleList pkg =
  let docsOutput = packageAsHtml pkg
      moduleNames = sort $ map (P.runModuleName . fst) $ htmlModules docsOutput
  in
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
  ereadme <- liftIO (GithubAPI.getReadme mtoken ghUser ghRepo ghTag)
  case ereadme of
    Right readme ->
      return (Just readme)
    Left err -> do
      $logError (tshow err)
      return Nothing

renderHtmlDocs :: D.VerifiedPackage -> String -> Handler (Maybe Html)
renderHtmlDocs pkg mnString =
  let docsOutput = packageAsHtml pkg
      mn = P.moduleNameFromString mnString
  in return $ map preEscapedToHtml $ lookup mn (htmlModules docsOutput)
