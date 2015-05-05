
module TemplateHelpers where

import Import
import qualified Data.Text as T
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import qualified Web.Bower.PackageMeta as Bower
import qualified Language.PureScript.Docs as D

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
