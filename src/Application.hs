-- To work around a bug in GHC 8.0.1:
-- See https://groups.google.com/forum/#!topic/yesodweb/DlyXqFM7ZnY
{-# LANGUAGE NoDisambiguateRecordFields #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- We disable the orphan instance warning because mkYesodDispatch defines an
-- orphan instance for the App type.
module Application
    ( getApplicationDev
    , appMain
    , develMain
    , makeFoundation
    -- * for DevelMain
    , getApplicationRepl
    , shutdownApp
    -- * for GHCI
    , handler
    ) where

import Import
import "monad-logger" Control.Monad.Logger (liftLoc)
import Language.Haskell.TH.Syntax (qLocation)
import Control.Concurrent (forkIO, threadDelay)
import Control.Parallel.Strategies (withStrategy)
import Network.Wai.Handler.Warp
  (Settings, defaultSettings, defaultShouldDisplayException, runSettings,
  setHost, setOnException, setPort, getPort)
import Network.Wai.Middleware.RequestLogger
  (Destination (Logger), IPAddrSource (..), OutputFormat (..), destination,
  mkRequestLogger, outputFormat)
import System.Log.FastLogger
  (defaultBufSize, newStdoutLoggerSet, toLogStr)
import qualified Yesod.Core.Unsafe as Unsafe

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Common
import Handler.Database
import Handler.Packages
import Handler.Search
import Handler.PackageBadges
import Handler.Help
import SearchIndex (emptySearchIndex, createSearchIndex, evalSearchIndex)

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- | This function allocates resources (such as a database connection pool),
-- performs initialization and return a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
    let mode = if isDevelopment then "development" else "production"
        appStatic = eStatic
    putStrLn $ "Starting in " <> mode <> " mode"
    appHttpManager <- newManager
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    appSearchIndex <- newTVarIO emptySearchIndex
    let foundation = App{..}
    void (startRegenThread foundation)
    return foundation

    where
    every interval action =
        forkIO (forever (action >> threadDelay interval))

    startRegenThread foundation =
       let hour = 60 * 60 * 1000 * 1000 -- microseconds
       in every hour $ do
           let emptySessionMap = mempty :: SessionMap
           pkgs <- Unsafe.runFakeHandler
                      emptySessionMap
                      appLogger
                      foundation
                      getAllPackages

           traverse ( atomically
                    . writeTVar (appSearchIndex foundation)
                    . withStrategy evalSearchIndex
                    . createSearchIndex
                    ) pkgs

-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applyng some additional middlewares.
makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <- mkRequestLogger def
        { outputFormat =
            if appDetailedRequestLogging $ appSettings foundation
                then Detailed True
                else Apache
                        (if appIpFromHeader $ appSettings foundation
                            then FromFallback
                            else FromSocket)
        , destination = Logger $ loggerSet $ appLogger foundation
        }

    -- Create the WAI application and apply middlewares
    appPlain <- toWaiAppPlain foundation
    return $ logWare $ defaultMiddlewaresNoLogging appPlain

-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
      setPort (appPort $ appSettings foundation)
    $ setHost (appHost $ appSettings foundation)
    $ setOnException (\_req e ->
        when (defaultShouldDisplayException e) $ messageLoggerSource
            foundation
            (appLogger foundation)
            $(qLocation >>= liftLoc)
            "yesod"
            LevelError
            (toLogStr $ "Exception from Warp: " ++ show e))
      defaultSettings

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app <- makeApplication foundation
    return (wsettings, app)

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
    -- Get settings from environment
    settings <- getAppSettings

    -- Generate the foundation from the settings
    foundation <- makeFoundation settings

    -- Generate a WAI Application from the foundation
    app <- makeApplication foundation

    -- Run the application with Warp
    runSettings (warpSettings foundation) app


--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app1 <- makeApplication foundation
    return (getPort wsettings, foundation, app1)

shutdownApp :: App -> IO ()
shutdownApp _ = return ()


---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------

-- | Run a handler
handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h
