
module GithubAPI where

import Import
import Text.Blaze.Html (preEscapedToHtml)
import qualified Control.Exception as E
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HashMap
import Text.XML.HXT.Core
import Data.CaseInsensitive (CI)
import qualified Language.PureScript.Docs as D

-- | Get a repository readme, rendered as HTML.
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
getReadme' mauth (D.GithubUser user) (D.GithubRepo repo) _ =
  let query = "" -- TODO: this will do for now; should really be ("ref=" ++ ref)
      headers = [("Accept", mediaTypeHtml)] ++ authHeader mauth
  in githubAPI ["repos", user, repo, "readme"] query headers

-- | Get the currently logged in user.
getUser :: GithubAuthToken -> IO (Either HttpException (Maybe D.GithubUser))
getUser token =
  (map . map) go (getUser' token)
  where
  go = map D.GithubUser . (loginFromJSON <=< A.decode)
  loginFromJSON val =
    case val of
      A.Object obj ->
        case HashMap.lookup "login" obj of
          Just (A.String t) -> Just $ unpack t
          _                 -> Nothing
      _            -> Nothing

getUser' :: GithubAuthToken -> IO (Either HttpException BL.ByteString)
getUser' auth =
  let headers = [("Accept", "application/json")] ++ authHeader (Just auth)
  in githubAPI ["user"] "" headers

githubAPI ::
  [String] -> -- ^ Path parts
  String -> -- ^ Query string
  [(CI ByteString, ByteString)] -> -- ^ Extra headers
  IO (Either HttpException BL.ByteString)
githubAPI path query extraHeaders = do
  tryHttp $ do
    initReq <- parseGithubUrlWithQuery path query
    let headers = [("User-Agent", "Pursuit")] ++ extraHeaders
    let req = initReq { requestHeaders = headers }
    withManager (responseBody <$> httpLbs req)

authHeader :: Maybe GithubAuthToken -> [(CI ByteString, ByteString)]
authHeader mauth =
   maybe []
         (\t -> [("Authorization", "bearer " <> runGithubAuthToken t)])
         mauth

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