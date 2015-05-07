
module GithubAPI where

import Import
import Text.Blaze.Html (preEscapedToHtml)
import qualified Control.Exception as E
import qualified Data.ByteString.Lazy as BL
import qualified Language.PureScript.Docs as D

mediaTypeHtml :: ByteString
mediaTypeHtml = "application/vnd.github.v3.html"

parseGithubUrlWithQuery :: MonadThrow m => [String] -> String -> m Request
parseGithubUrlWithQuery parts query =
  parseUrl $ concat [ "https://api.github.com/"
                    , intercalate "/" parts
                    , "?"
                    , query
                    ]

tryHttp :: IO a -> IO (Either HttpException a)
tryHttp = E.try

getRenderedReadme ::
  Maybe GithubAuthToken ->
  D.GithubUser ->
  D.GithubRepo ->
  String -> -- ^ ref: commit, branch, etc.
  IO (Either HttpException Html)
getRenderedReadme mauth (D.GithubUser user) (D.GithubRepo repo) ref =
  tryHttp $ do
    initReq <- parseGithubUrlWithQuery ["repos", user, repo, "readme"]
                                       ("ref=" ++ ref)
    let headers =
          [ ("User-Agent", "Pursuit")
          , ("Accept", mediaTypeHtml)
          ] ++ maybe []
                     (\t -> [("Authorization", "bearer " <> runGithubAuthToken t)])
                     mauth
    let req = initReq { requestHeaders = headers }

    withManager $
      preEscapedToHtml . decodeUtf8 . responseBody <$> httpLbs req
