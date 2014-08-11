module Main where

import Control.AJAX

import qualified Data.Trie as T

main = do
  get "data.json" $ \json -> do
    Debug.Trace.trace json
