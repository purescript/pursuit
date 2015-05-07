
module GithubAPI where

import Import
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

getRenderedReadme ::
  GithubAuthToken ->
  D.GithubUser ->
  D.GithubRepo ->
  String -> -- ^ ref: commit, branch, etc.
  IO (Maybe BL.ByteString)
getRenderedReadme auth (D.GithubUser user) (D.GithubRepo repo) ref =
  trySome performRequest <#> either (const Nothing) Just
  where
  trySome :: IO a -> IO (Either E.SomeException a)
  trySome = E.try 

  (<#>) = flip (<$>)

  performRequest = do
    initReq <- parseGithubUrlWithQuery ["repos", user, repo, "readme"]
                                       ("ref=" ++ ref)
    let headers =  [ ("User-Agent", "Pursuit")
                   , ("Authorization", "bearer " <> runGithubAuthToken auth)
                   , ("Accept", mediaTypeHtml)
                   ]
    let req = initReq { requestHeaders = headers }

    withManager $ do
      resp <- httpLbs req
      return (responseBody resp)
