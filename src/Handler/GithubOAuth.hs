
module Handler.GithubOAuth
  ( startFlow
  , getOAuthCallbackR
  , requireAuthentication
  ) where

import Import
import qualified Data.Aeson as A
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.HashMap.Strict as HashMap
import qualified Language.PureScript.Docs as D

import Handler.Utils
import qualified GithubAPI

requireAuthentication :: (D.GithubUser -> Handler a) -> Handler a
requireAuthentication cont = do
  muser <- getSessionUser
  case muser of
    Just user -> cont user
    Nothing -> do
      setUltDestCurrent
      startFlow

startFlow :: Handler a
startFlow = do
  state <- Base64.encode <$> generateBytes stateLength

  setSession keyGithubOAuthState (decodeUtf8 state)
  url <- getGithubRedirectUrl state
  redirect url

getOAuthCallbackR :: Handler Html
getOAuthCallbackR = do
  stateParam   <- requireParam "state"
  stateSession <- lookupSession keyGithubOAuthState
  when (stateSession /= Just stateParam) $
    badRequest "State mismatch"

  deleteSession keyGithubOAuthState

  code <- requireParam "code"
  mtoken <- tryExchangeToken (encodeUtf8 code)
  case mtoken of
    Nothing -> badRequest "Invalid code"
    Just token -> do
      eResult <- GithubAPI.getUser token
      case eResult of
        Left err -> do
          $logError (tshow err)
          internalServerError
        Right Nothing -> do
          $logError "Unable to obtain user login from Github API response"
          internalServerError
        Right (Just user) -> do
          setSessionUser user
          setSession keyGithubAuthToken $ decodeUtf8 $ runGithubAuthToken token
          redirectUltDest HomeR

tryExchangeToken :: ByteString -> Handler (Maybe GithubAuthToken)
tryExchangeToken code = do
  url <- getGithubExchangeTokenUrl code
  liftIO $ do
    initReq <- parseUrlThrow (unpack url)
    let req = initReq { method = "POST"
                      , requestHeaders = [("Accept", "application/json")]
                      }
    body <- withManager (responseBody <$> httpLbs req)
    return (A.decode body >>= tokenFromJSON)

tokenFromJSON :: A.Value -> Maybe GithubAuthToken
tokenFromJSON val =
  case val of
    A.Object obj ->
      case HashMap.lookup "access_token" obj of
        Just (A.String t) -> Just $ GithubAuthToken $ encodeUtf8 t
        _ -> Nothing
    _ -> Nothing

requireParam :: Text -> Handler Text
requireParam name = do
  p <- lookupGetParam name
  case p of
    Just p' -> return p'
    Nothing -> badRequest ("Required parameter \"" <> name <> "\" was missing")

getGithubRedirectUrl :: ByteString -> Handler Text
getGithubRedirectUrl state = do
  clientID <- getClientID
  let query = renderSimpleQuery True
                [ ("client_id", clientID)
                , ("state", state)
                ]
  return ("https://github.com/login/oauth/authorize" <> decodeUtf8 query)

getGithubExchangeTokenUrl :: ByteString -> Handler Text
getGithubExchangeTokenUrl code = do
  clientID     <- getClientID
  clientSecret <- getClientSecret
  let query = renderSimpleQuery True
                [ ("code", code)
                , ("client_id", clientID)
                , ("client_secret", clientSecret)
                ]
  return ("https://github.com/login/oauth/access_token" <> decodeUtf8 query)

getClientID :: Handler ByteString
getClientID = appGithubClientID . appSettings <$> getYesod

getClientSecret :: Handler ByteString
getClientSecret = appGithubClientSecret . appSettings <$> getYesod

-- | Length of the "state" parameter, in bytes.
stateLength :: Int
stateLength = 60

-- | Session key for the state parameter for a Github OAuth authentication
-- flow.
keyGithubOAuthState :: Text
keyGithubOAuthState = "github_oauth_state"

-- | Session key for an authenticated github user.
keyGithubUser :: Text
keyGithubUser = "github_user"

getSessionUser :: Handler (Maybe D.GithubUser)
getSessionUser =
  (map . map) (D.GithubUser . unpack) (lookupSession keyGithubUser)

setSessionUser :: D.GithubUser -> Handler ()
setSessionUser =
  setSession keyGithubUser . pack . D.runGithubUser

-- | Session key for the access token for an authenticated github user.
keyGithubAuthToken :: Text
keyGithubAuthToken = "github_auth_token"
