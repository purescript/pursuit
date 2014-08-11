module Main where

import qualified Data.Trie as T

main = Debug.Trace.print $ 
  Data.Trie.lookupAll "hel" $ 
    Data.Trie.insert "hello" 10 $ 
      Data.Trie.insert "hell" 9 $
        Data.Trie.empty
