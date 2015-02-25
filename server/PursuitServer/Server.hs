{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PursuitServer.Server where

import Data.Char (toLower)
import Data.List (foldl')
import Data.Maybe
import qualified Data.Trie as T
import qualified Data.Text.Lazy as TL

import Control.Monad (void, forever)

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM
import Control.Monad.IO.Class

import Web.Scotty
import Network.Wai.Middleware.Static

import System.Exit (exitFailure)

import Pursuit
import Pursuit.Generator

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
          let result = query q db
          renderTemplate (index (Just result))
        _ ->
          renderTemplate (index Nothing)


safeParam :: Parsable a => TL.Text -> ActionM (Maybe a)
safeParam name = fmap Just (param name) `rescue` const (return Nothing)

-- serve static files from a particular directory
serveStaticFiles :: String -> ScottyM ()
serveStaticFiles = middleware . staticPolicy . addBase

buildLookup :: [PursuitEntry] -> T.Trie PursuitEntry
buildLookup = foldl' (\t e -> T.insert (map toLower (entryName e)) e t) T.empty

-- Generate the database for the first time, return it as a TVar, and also
-- kick off a thread to rebuild it periodically.
--
-- If the first attempt to rebuild the database fails, exit the program.
startGenerateThread :: FilePath -> IO (TVar (T.Trie PursuitEntry))
startGenerateThread librariesFile = do
  tvar <- newTVarIO T.empty
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
      (warnings, _, Right (PursuitDatabase _ entries)) -> do
        atomically (writeTVar tvar (buildLookup entries))
        if (null warnings)
          then putStrLn "Done. No warnings."
          else mapM_ (putStrLn . show) warnings

hourly :: IO a -> IO a
hourly action = forever (action >> threadDelay (3600 * 1000000))

query :: String -> T.Trie PursuitEntry -> [PursuitEntry]
query q = fromMaybe [] . fmap (take 20 . map snd . T.toArray) . T.lookupAll (map toLower q)
