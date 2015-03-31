{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PursuitServer.Server where

import Data.Monoid
import qualified Data.Text.Lazy as TL

import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)

import Control.Monad (void, forever)
import Control.Applicative ((<$>))

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM
import Control.Monad.IO.Class

import Web.Scotty
import Network.Wai.Middleware.Static

import Github.Auth (GithubAuth(GithubOAuth))

import System.Exit (exitFailure)

import Pursuit

import PursuitServer.Types
import PursuitServer.HtmlTemplates

runServer :: ServerOptions -> IO ()
runServer (ServerOptions {..}) = do
  dbvar <- startGenerateThread serverLibrariesFile (GithubOAuth <$> serverGithubAuthToken)

  scotty serverPort $ do
    serveStaticFiles "static"

    get "/" $ do
      safeParam "q" >>= \case
        Just q -> do
          db <- liftIO $ readTVarIO dbvar
          let result = runQuery (queryDeclsJ q) db
          renderTemplate (index (Just (q, result)))
        _ ->
          renderTemplate (index Nothing)


safeParam :: Parsable a => TL.Text -> ActionM (Maybe a)
safeParam name = fmap Just (param name) `rescue` const (return Nothing)

-- serve static files from a particular directory
serveStaticFiles :: String -> ScottyM ()
serveStaticFiles = middleware . staticPolicy . addBase

-- Generate the database for the first time, return it as a TVar, and also
-- kick off a thread to rebuild it periodically.
--
-- If the first attempt to rebuild the database fails, exit the program.
startGenerateThread :: FilePath -> Maybe GithubAuth -> IO (TVar PursuitDatabase)
startGenerateThread librariesFile githubAuth = do
  tvar <- newTVarIO mempty
  putStrLn "Building database..."
  buildDb tvar (\err -> do putStrLn err
                           exitFailure)

  void $ forkIO $ hourly $ do
    putStrLnWithTime "Regenerating database..."
    buildDb tvar (\err -> do putStrLn "failed to rebuild database:"
                             putStrLn err)

  return tvar

  where
  -- Build the database, put it in the supplied tvar, and also allow the user
  -- to pass a callback in case an error occurs.
  buildDb tvar onError =
    generateDatabase librariesFile githubAuth >>= \case
      (warnings, logs, eitherDb) -> do
        if (null warnings)
          then putStrLn "Done. No warnings."
          else printAll warnings
        printAll logs

        case eitherDb of
          Left err -> onError (show err)
          Right db -> atomically (writeTVar tvar db)

  printAll :: Show a => [a] -> IO ()
  printAll = mapM_ (putStrLn . show)

hourly :: IO a -> IO a
hourly action = forever (threadDelay (3600 * 1000000) >> action)

getTimestamp :: IO String
getTimestamp = do
  time <- getCurrentTime
  return (formatTime defaultTimeLocale "[%Y-%m-%d %H:%M:%S]" time)

putStrLnWithTime :: String -> IO ()
putStrLnWithTime str = do
  t <- getTimestamp
  putStrLn (t ++ " " ++ str)
