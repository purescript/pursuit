
module GithubAPI
  ( getReadme
  , getUser
  , ReadmeMissing(..)
  ) where

import Import
import Text.Blaze.Html (preEscapedToHtml)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.CaseInsensitive as CI
import Text.HTML.SanitizeXSS (sanitize)
import Data.CaseInsensitive (CI)
import qualified Language.PureScript.Docs as D
import qualified Network.HTTP.Types as HTTP

import qualified XMLArrows

data ReadmeMissing
  = APIRateLimited
  | ReadmeNotFound
  | OtherReason HttpException

-- | Try to determine why a readme was not available
diagnoseReadmeProblem :: HttpException -> ReadmeMissing
diagnoseReadmeProblem = \case
  HttpExceptionRequest _ (StatusCodeException resp _)
    | lookup (CI.mk "X-RateLimit-Remaining") (responseHeaders resp) == Just "0"
      && (responseStatus resp) == forbidden403 ->
    APIRateLimited
    | responseStatus resp == notFound404 ->
    ReadmeNotFound
  r ->
    OtherReason r

-- | Get a repository readme, rendered as HTML.
getReadme ::
  (MonadUnliftIO m, MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) =>
  Maybe GithubAuthToken ->
  D.GithubUser ->
  D.GithubRepo ->
  Text -> -- ref: commit, branch, etc.
  m (Either ReadmeMissing Html)
getReadme mauth user repo ref = do
  readme <- getReadme' mauth user repo ref
  pure $ bimap diagnoseReadmeProblem treatHtml readme
  where
  treatHtml =
    decodeUtf8
    >>> unpack
    >>> XMLArrows.runString arrow
    >>> pack
    >>> sanitize
    >>> preEscapedToHtml

  arrow =
    XMLArrows.stripH1
    >>> XMLArrows.makeRelativeLinksAbsolute
          "a" "href" (unpack (buildGithubURL user repo ref))
    >>> XMLArrows.makeRelativeLinksAbsolute
          "img" "src" (unpack (buildRawGithubURL user repo ref))

buildGithubURL :: D.GithubUser -> D.GithubRepo -> Text -> Text
buildGithubURL (D.GithubUser user) (D.GithubRepo repo) ref =
    concat ["https://github.com/", user, "/", repo, "/blob/", ref, "/"]

buildRawGithubURL :: D.GithubUser -> D.GithubRepo -> Text -> Text
buildRawGithubURL (D.GithubUser user) (D.GithubRepo repo) ref =
    concat ["https://raw.githubusercontent.com/", user, "/", repo, "/", ref, "/"]

getReadme' ::
  (MonadUnliftIO m, MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) =>
  Maybe GithubAuthToken ->
  D.GithubUser ->
  D.GithubRepo ->
  Text -> -- ref: commit, branch, etc.
  m (Either HttpException BL.ByteString)
getReadme' mauth (D.GithubUser user) (D.GithubRepo repo) ref =
  let query = "ref=" ++ ref
      headers = ("Accept", mediaTypeHtml) : authHeader mauth
  in githubAPI ["repos", user, repo, "readme"] query headers

-- | Get the currently logged in user.
getUser ::
  (MonadUnliftIO m, MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) =>
  GithubAuthToken -> m (Either HttpException (Maybe D.GithubUser))
getUser token =
  (map . map) extractUser (getUser' token) >>= catch401
  where
  extractUser = map D.GithubUser . (loginFromJSON <=< A.decode)
  loginFromJSON val =
    case val of
      A.Object obj ->
        case KM.lookup "login" obj of
          Just (A.String t) -> Just t
          _                 -> Nothing
      _            -> Nothing

  catch401 (Left (HttpExceptionRequest _ (StatusCodeException resp _)))
    | responseStatus resp == HTTP.unauthorized401 = return $ Right Nothing
  catch401 other = return other

getUser' ::
  (MonadUnliftIO m, MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) =>
  GithubAuthToken -> m (Either HttpException BL.ByteString)
getUser' auth =
  githubAPI ["user"] "" headers
  where
  headers = ("Accept", "application/json") : authHeader (Just auth)

githubAPI ::
  (MonadUnliftIO m, MonadThrow m, MonadIO m, HasHttpManager env, MonadReader env m) =>
  [Text] -> -- Path parts
  Text -> -- Query string
  [(CI ByteString, ByteString)] -> -- Extra headers
  m (Either HttpException BL.ByteString)
githubAPI path query extraHeaders = do
  tryHttp $ do
    initReq <- parseGithubUrlWithQuery path query
    let headers = [("User-Agent", "Pursuit")] ++ extraHeaders
    let req = initReq { requestHeaders = headers }
    liftM responseBody $ httpLbs req

authHeader :: Maybe GithubAuthToken -> [(CI ByteString, ByteString)]
authHeader mauth =
   maybe []
         (\t -> [("Authorization", "bearer " <> runGithubAuthToken t)])
         mauth

mediaTypeHtml :: ByteString
mediaTypeHtml = "application/vnd.github.v3.html"

parseGithubUrlWithQuery :: MonadThrow m => [Text] -> Text -> m Request
parseGithubUrlWithQuery parts query =
  parseUrlThrow $ unpack $ concat
    [ "https://api.github.com/"
    , intercalate "/" parts
    , "?"
    , query
    ]

tryHttp :: (MonadUnliftIO m, MonadThrow m) => m a -> m (Either HttpException a)
tryHttp = try
