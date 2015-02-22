{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PursuitServer.Server where

import Data.Char (toLower)
import Data.List (foldl')
import Data.Monoid
import qualified Data.Trie as T

import Control.Monad (void, forever)

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM

import Web.Scotty
import Network.Wai.Middleware.Static

import Pursuit
import Pursuit.Generator

import PursuitServer.Types
import PursuitServer.HtmlTemplates

runServer :: ServerOptions -> IO ()
runServer (ServerOptions {..}) = do
  db <- startGenerateThread serverLibrariesFile

  scotty serverPort $ do
    serveStaticFiles "static"

    get "/" $ do
      -- q <- param "q"
      -- json $ query q db
      renderTemplate index

-- serve static files from a particular directory
serveStaticFiles :: String -> ScottyM ()
serveStaticFiles = middleware . staticPolicy . addBase

buildLookup :: [PursuitEntry] -> T.Trie PursuitEntry
buildLookup = foldl' (\t e -> T.insert (map toLower (entryName e)) e t) T.empty

startGenerateThread :: FilePath -> IO (TVar PursuitDatabase)
startGenerateThread librariesFile = do
  tvar <- newTVarIO mempty

  void $ forkIO $ hourly $ do
    putStrLn "Regenerating database..."

    generateDatabase librariesFile >>= \case
      Left err -> putStrLn (show err)
      Right (db, warnings) -> do
        atomically (writeTVar tvar db)
        if (null warnings)
          then putStrLn "Done. No warnings."
          else mapM_ (putStrLn . show) warnings

  return tvar

hourly :: IO a -> IO a
hourly action = forever (action >> threadDelay (3600 * 1000000))

query :: String -> T.Trie PursuitEntry -> Maybe [PursuitEntry]
query q = fmap (take 20 . map snd . T.toArray) . T.lookupAll (map toLower q)
