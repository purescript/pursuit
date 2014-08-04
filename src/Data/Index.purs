module Data.Index where

import Data.Tuple

import qualified Data.Map as M

newtype Module = Module
  { name     :: String
  , uri      :: String
  , desc     :: String
  }

newtype Value = Value
  { name     :: String
  , desc     :: String
  , mname    :: String
  }

newtype Index = Index 
  { byName   :: M.Map String Value
  , byModule :: M.Map String Module
  }

newtype Result = Result
  { name     :: String
  , desc     :: String
  }

buildIndex :: [Value] -> [Module] -> Index
buildIndex vals ms = Index { byName: M.fromList (valueToPair <$> vals)
                           , byModule: M.fromList (moduleToPair <$> ms) 
                           }
  where
  valueToPair v@(Value { name = k }) = Tuple k v
  moduleToPair v@(Module { name = k }) = Tuple k v

search :: String -> [Result]
search _ = []
