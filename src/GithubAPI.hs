
module GithubAPI where

import Import
import Text.Blaze.Html (preEscapedToHtml)
import qualified Control.Exception as E
import qualified Data.ByteString.Lazy as BL
import Text.XML.HXT.Core
import qualified Language.PureScript.Docs as D

getReadme ::
  Maybe GithubAuthToken ->
  D.GithubUser ->
  D.GithubRepo ->
  String -> -- ^ ref: commit, branch, etc.
  IO (Either HttpException Html)
getReadme mauth user repo ref =
  (map . map) go (getReadme' mauth user repo ref)
  where
  go = preEscapedToHtml . stripH1 . unpack . decodeUtf8

getReadme' ::
  Maybe GithubAuthToken ->
  D.GithubUser ->
  D.GithubRepo ->
  String -> -- ^ ref: commit, branch, etc.
  IO (Either HttpException BL.ByteString)
getReadme' mauth (D.GithubUser user) (D.GithubRepo repo) ref =
  tryHttp $ do
    initReq <- parseGithubUrlWithQuery ["repos", user, repo, "readme"]
                                       "" -- this will do for now.
                                          -- should really be:
                                          -- ("ref=" ++ ref)
    let headers =
          [ ("User-Agent", "Pursuit")
          , ("Accept", mediaTypeHtml)
          ] ++ maybe []
                     (\t -> [("Authorization", "bearer " <> runGithubAuthToken t)])
                     mauth
    let req = initReq { requestHeaders = headers }

    withManager $
      responseBody <$> httpLbs req

stripH1 :: String -> String
stripH1 = unsafeHead . runLA stripH1Arrow
  where
  stripH1Arrow =
    hread >>>
      processTopDown (neg (hasName "h1") `guards` this) >>>
      writeDocumentToString []

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
