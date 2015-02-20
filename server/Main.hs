{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Pursuit
import Pursuit.Generator

import Data.Char (toLower)
import Data.Version (showVersion)
import Data.List (foldl')

import qualified Data.Trie as T

import Control.Applicative

import System.Exit (exitFailure)
import System.IO (hPutStr, stderr)
import Web.Scotty

import qualified Paths_pursuit as Paths

buildLookup :: [PursuitEntry] -> T.Trie PursuitEntry
buildLookup = foldl' (\t e -> T.insert (map toLower (entryName e)) e t) T.empty

query :: String -> T.Trie PursuitEntry -> Maybe [PursuitEntry]
query q = fmap (take 20 . map snd . T.toArray) . T.lookupAll (map toLower q)

runServer :: Int -> FilePath -> IO ()
runServer portNumber path =
  generateDatabase path >>= \case
    Right ((PursuitDatabase _ entries), _) -> do
      let db = buildLookup entries
      scotty portNumber $ do
        get "/" $ do
          q <- param "q"
          json $ query q db
    Left err -> do
      hPutStr stderr (show err)
      exitFailure

-- port :: Term Int
-- port = value $ opt 8080 $ (optInfo [ "p", "port" ]) { optDoc = "The port to listen on" }

-- datafile :: Term FilePath
-- datafile = value $ opt "libraries.json" $ (optInfo [ "d", "data" ]) { optDoc = "The data file" }

-- term :: Term (IO ())
-- term = runServer <$> port <*> datafile

-- termInfo :: TermInfo
-- termInfo = defTI
--   { termName = "pursuit-server"
--   , version  = showVersion Paths.version
--   , termDoc  = "Start a web server to service for Pursuit database requests"
--   }

-- main :: IO ()
-- main = run (term, termInfo)

main :: IO ()
main = runServer 8080 "libraries.json"
