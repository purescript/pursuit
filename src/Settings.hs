-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings where

import ClassyPrelude.Yesod
import qualified Data.Aeson as A
import System.Environment          (lookupEnv)
import Language.Haskell.TH.Syntax  (Exp, Name, Q)
import Network.Wai.Handler.Warp    (HostPreference)
import Yesod.Default.Util          (WidgetFileSettings, widgetFileNoReload,
                                    widgetFileReload)

newtype GithubAuthToken =
  GithubAuthToken { runGithubAuthToken :: ByteString }
  deriving (Show, Eq, Ord)

-- | Runtime settings to configure this application. These settings can be
-- loaded from various sources: defaults, environment variables, config files,
-- theoretically even a database.
data AppSettings = AppSettings
    { appStaticDir              :: String
    -- ^ Directory from which to serve static files.
    , appRoot                   :: Text
    -- ^ Base for all generated URLs.
    , appHost                   :: HostPreference
    -- ^ Host/interface the server should bind to.
    , appPort                   :: Int
    -- ^ Port to listen on
    , appIpFromHeader           :: Bool
    -- ^ Get the IP address from the header when logging. Useful when sitting
    -- behind a reverse proxy.

    , appDetailedRequestLogging :: Bool
    -- ^ Use detailed request logging system
    , appShouldLogAll           :: Bool
    -- ^ Should all log messages be displayed?
    , appReloadTemplates        :: Bool
    -- ^ Use the reload version of templates
    , appMutableStatic          :: Bool
    -- ^ Assume that files in the static dir may change after compilation
    , appSkipCombining          :: Bool
    -- ^ Perform no stylesheet/script combining

    -- Example app-specific configuration values.
    , appAnalytics              :: Maybe Text
    -- ^ Google Analytics code
    , appGithubAuthToken        :: Maybe GithubAuthToken
    -- ^ Github OAuth token (for fetching READMEs).
    , appDataDir                :: String
    -- ^ Directory where package data is kept.
    , appGithubClientID         :: ByteString
    -- ^ GitHub OAuth client ID
    , appGithubClientSecret     :: ByteString
    -- ^ GitHub OAuth client secret
    }

isDevelopment :: Bool
#if DEVELOPMENT
isDevelopment = True
#else
isDevelopment = False
#endif

getAppSettings :: IO AppSettings
getAppSettings = do
  let appDetailedRequestLogging = isDevelopment
  let appShouldLogAll           = isDevelopment
  let appReloadTemplates        = isDevelopment
  let appMutableStatic          = isDevelopment
  let appSkipCombining          = isDevelopment

  appStaticDir    <- env "STATIC_DIR" .!= "./static"
  appRoot         <- env "APPROOT" .!= "http://localhost:3000"
  appHost         <- fromString <$> env "HOST" .!= "*4"
  appPort         <- env "PORT" .!= 3000
  appIpFromHeader <- env "IP_FROM_HEADER" .!= False

  appAnalytics <- env "GOOGLE_ANALYTICS_CODE"
  appDataDir   <- env "DATA_DIR" .!= "./data"

  appGithubAuthToken    <- map (GithubAuthToken . fromString) <$> env "GITHUB_AUTH_TOKEN"
  appGithubClientID     <- fromString <$> env' "GITHUB_CLIENT_ID"
  appGithubClientSecret <- fromString <$> env' "GITHUB_CLIENT_SECRET"

  return AppSettings {..}

  where
  env  = lookupEnvironment . ("PURSUIT_" ++)
  env' = getEnvironment    . ("PURSUIT_" ++)

  (.!=) :: (Functor f) => f (Maybe a) -> a -> f a
  action .!= def' = fromMaybe def' <$> action

lookupEnvironment :: (A.FromJSON a) => String -> IO (Maybe a)
lookupEnvironment var = do
  mstr <- lookupEnv var
  case mstr of
    Nothing -> return Nothing
    Just str -> case parseString str of
      Right val -> return (Just val)
      Left err -> error $ "Failed to parse environment variable" ++
                          " \"" ++ var ++ "\": " ++ err
  where
  parseString str = case A.fromJSON (A.String (pack str)) of
    A.Success x -> Right x
    A.Error err -> Left err

getEnvironment :: (A.FromJSON a) => String -> IO a
getEnvironment var = do
  r <- lookupEnvironment var
  case r of
    Just r' -> return r'
    Nothing -> error $ "Required environment variable \"" ++ var ++ "\" " ++
                       "is not set"

-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
--
-- For more information on modifying behavior, see:
--
-- https://github.com/yesodweb/yesod/wiki/Overriding-widgetFile
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def

-- | How static files should be combined.
combineSettings :: CombineSettings
combineSettings = def

-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: String -> Q Exp
widgetFile = (if isDevelopment
                then widgetFileReload
                else widgetFileNoReload)
              widgetFileSettings

-- The following two functions can be used to combine multiple CSS or JS files
-- at compile time to decrease the number of http requests.
-- Sample usage (inside a Widget):
--
-- > $(combineStylesheets 'StaticR [style1_css, style2_css])

combineStylesheets :: Name -> [Route Static] -> Q Exp
combineStylesheets = combineStylesheets'
    isDevelopment
    combineSettings

combineScripts :: Name -> [Route Static] -> Q Exp
combineScripts = combineScripts'
    isDevelopment
    combineSettings
