
module GithubAPI
  ( getReadme
  , getUser
  ) where

import Import
import Text.Blaze.Html (preEscapedToHtml)
import qualified Control.Monad.Catch as Catch
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HashMap
import Text.XML.HXT.Core as HXT
import Text.HTML.SanitizeXSS (sanitize)
import Data.CaseInsensitive (CI)
import qualified Language.PureScript.Docs as D
import qualified Network.HTTP.Types as HTTP

-- | Get a repository readme, rendered as HTML.
getReadme ::
  (MonadCatch m, MonadIO m, HasHttpManager env, MonadReader env m) =>
  Maybe GithubAuthToken ->
  D.GithubUser ->
  D.GithubRepo ->
  String -> -- ^ ref: commit, branch, etc.
  m (Either HttpException Html)
getReadme mauth user repo ref =
  (liftM . liftM) go (getReadme' mauth user repo ref)
  where
  go =
    decodeUtf8
    >>> unpack
    >>> runXmlArrow arrow
    >>> pack
    >>> sanitize
    >>> preEscapedToHtml

  arrow =
    stripH1
    >>> makeRelativeLinksAbsolute "a" "href" (buildGithubURL user repo ref)
    >>> makeRelativeLinksAbsolute "img" "src" (buildRawGithubURL user repo ref)

buildGithubURL :: D.GithubUser -> D.GithubRepo -> String -> String
buildGithubURL (D.GithubUser user) (D.GithubRepo repo) ref =
    concat ["https://github.com/", user, "/", repo, "/blob/", ref, "/"]

buildRawGithubURL :: D.GithubUser -> D.GithubRepo -> String -> String
buildRawGithubURL (D.GithubUser user) (D.GithubRepo repo) ref =
    concat ["https://raw.githubusercontent.com/", user, "/", repo, "/", ref, "/"]

getReadme' ::
  (MonadCatch m, MonadIO m, HasHttpManager env, MonadReader env m) =>
  Maybe GithubAuthToken ->
  D.GithubUser ->
  D.GithubRepo ->
  String -> -- ^ ref: commit, branch, etc.
  m (Either HttpException BL.ByteString)
getReadme' mauth (D.GithubUser user) (D.GithubRepo repo) ref =
  let query = "ref=" ++ ref
      headers = ("Accept", mediaTypeHtml) : authHeader mauth
  in githubAPI ["repos", user, repo, "readme"] query headers

-- | Get the currently logged in user.
getUser ::
  (MonadCatch m, MonadIO m, HasHttpManager env, MonadReader env m, Functor m) =>
  GithubAuthToken -> m (Either HttpException (Maybe D.GithubUser))
getUser token =
  (map . map) extractUser (getUser' token) >>= catch401
  where
  extractUser = map D.GithubUser . (loginFromJSON <=< A.decode)
  loginFromJSON val =
    case val of
      A.Object obj ->
        case HashMap.lookup "login" obj of
          Just (A.String t) -> Just $ unpack t
          _                 -> Nothing
      _            -> Nothing

  catch401 (Left (StatusCodeException status _ _))
    | status == HTTP.unauthorized401 = return $ Right Nothing
  catch401 other = return other

getUser' ::
  (MonadCatch m, MonadIO m, HasHttpManager env, MonadReader env m) =>
  GithubAuthToken -> m (Either HttpException BL.ByteString)
getUser' auth =
  githubAPI ["user"] "" headers
  where
  headers = ("Accept", "application/json") : authHeader (Just auth)

githubAPI ::
  (MonadCatch m, MonadIO m, HasHttpManager env, MonadReader env m) =>
  [String] -> -- ^ Path parts
  String -> -- ^ Query string
  [(CI ByteString, ByteString)] -> -- ^ Extra headers
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

runXmlArrow :: LA XmlTree XmlTree -> String -> String
runXmlArrow arrow =
  unsafeHead . runLA (hread >>> arrow >>> writeDocumentToString [])

stripH1 :: LA XmlTree XmlTree
stripH1 =
  processTopDown (neg (hasName "h1") `guards` this)

-- | Make all relative links into absolute links by providing a base URL.
makeRelativeLinksAbsolute ::
  String    -- ^ Tag name to modify
  -> String -- ^ Attribute name to modify
  -> String -- ^ Base URL to use for relative links
  -> LA XmlTree XmlTree
makeRelativeLinksAbsolute tagName attrName base =
  processTopDown $
    processAttrl (changeAttrValue (mkAbs base) `HXT.when` hasName attrName)
      `HXT.when` (isElem >>> hasName tagName)

  where
  mkAbs base' url = fromMaybe url $ expandURIString url $ base'

mediaTypeHtml :: ByteString
mediaTypeHtml = "application/vnd.github.v3.html"

parseGithubUrlWithQuery :: MonadThrow m => [String] -> String -> m Request
parseGithubUrlWithQuery parts query =
  parseUrlThrow $ concat [ "https://api.github.com/"
                         , intercalate "/" parts
                         , "?"
                         , query
                         ]

tryHttp :: MonadCatch m => m a -> m (Either HttpException a)
tryHttp = Catch.try
