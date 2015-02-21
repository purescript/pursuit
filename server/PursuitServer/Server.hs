{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PursuitServer.Server where

import Data.Char (toLower)
import Data.List (foldl')
import Data.Monoid
import qualified Data.Trie as T

import Control.Concurrent.STM

import System.IO (hPutStr, stderr)
import System.Exit (exitFailure)

import Web.Scotty

import Pursuit
import Pursuit.Generator

import PursuitServer.Types
import PursuitServer.HtmlTemplates

runServer :: ServerOptions -> IO ()
runServer (ServerOptions {..}) = do
  db <- startGenerateThread serverLibrariesFile

  scotty serverPort $ do
    get "/" $ do
      renderTemplate index
      -- q <- param "q"
      -- json $ query q db

buildLookup :: [PursuitEntry] -> T.Trie PursuitEntry
buildLookup = foldl' (\t e -> T.insert (map toLower (entryName e)) e t) T.empty

startGenerateThread :: FilePath -> IO (TVar PursuitDatabase)
startGenerateThread _ =
  newTVarIO mempty

query :: String -> T.Trie PursuitEntry -> Maybe [PursuitEntry]
query q = fmap (take 20 . map snd . T.toArray) . T.lookupAll (map toLower q)
