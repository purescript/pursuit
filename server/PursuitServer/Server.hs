{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PursuitServer.Server where

import Data.Monoid
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T

import Control.Monad (void, forever)

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM
import Control.Monad.IO.Class

import Web.Scotty
import Network.Wai.Middleware.Static

import System.Exit (exitFailure)

import Pursuit

import PursuitServer.Types
import PursuitServer.HtmlTemplates

runServer :: ServerOptions -> IO ()
runServer (ServerOptions {..}) = do
  dbvar <- startGenerateThread serverLibrariesFile

  scotty serverPort $ do
    serveStaticFiles "static"

    get "/" $ do
      safeParam "q" >>= \case
        Just q -> do
          db <- liftIO $ readTVarIO dbvar
          let result = runQuery (queryDeclsJ q) db
          renderTemplate (index (Just (T.pack q, result)))
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
startGenerateThread :: FilePath -> IO (TVar PursuitDatabase)
startGenerateThread librariesFile = do
  tvar <- newTVarIO mempty
  putStrLn "Building database..."
  buildDb tvar (\err -> do putStrLn err
                           exitFailure)

  void $ forkIO $ hourly $ do
    putStrLn "Regenerating database..."
    buildDb tvar (\err -> do putStrLn "failed to rebuild database:"
                             putStrLn err)

  return tvar

  where
  -- Build the database, put it in the supplied tvar, and also allow the user
  -- to pass a callback in case an error occurs.
  buildDb tvar onError =
    generateDatabase librariesFile >>= \case
      (_, _, Left err) -> onError (show err)
      (warnings, _, Right db) -> do
        atomically (writeTVar tvar db)
        if (null warnings)
          then putStrLn "Done. No warnings."
          else mapM_ (putStrLn . show) warnings

hourly :: IO a -> IO a
hourly action = forever (action >> threadDelay (3600 * 1000000))
