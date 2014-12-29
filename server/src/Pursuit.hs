{-# LANGUAGE OverloadedStrings #-}

module Pursuit where

import Data.Monoid

import Data.Aeson ((.=), (.:), (.:?))
import qualified Data.Aeson as A

import qualified Data.Map as M

import Control.Applicative

type GitUrl = String

-- The structure of one entry in the input libraries.json file.
data Library = Library { libraryGitUrl :: GitUrl
                       , libraryBowerName :: Maybe String
                       }

instance A.FromJSON Library where
  parseJSON (A.Object o) =
    Library <$> o .: "gitUrl"
            <*> o .:? "bowerName"
  parseJSON val = fail $ "couldn't parse " ++ show val ++ " as Library"

-- The data associated with a named library in the output.
data LibraryInfo = LibraryInfo { infoVersion :: String }

instance A.FromJSON LibraryInfo where
  parseJSON (A.Object o) =
    LibraryInfo <$> o .: "version"
  parseJSON val = fail $ "couldn't parse " ++ show val ++ " as LibraryInfo"

instance A.ToJSON LibraryInfo where
  toJSON (LibraryInfo vers) =
    A.object [ "version" .= vers
             ]

data PursuitEntry =
  PursuitEntry { entryName           :: String
               , entryModule         :: String
               , entryDetail         :: String
               , entryLibraryName    :: Maybe String
               }
               deriving (Show, Eq)

instance A.FromJSON PursuitEntry where
  parseJSON (A.Object o) =
    PursuitEntry <$> o .: "name"
                 <*> o .: "module"
                 <*> o .: "detail"
                 <*> o .:? "library"
  parseJSON val = fail $ "couldn't parse " ++ show val ++ " as PursuitEntry"

instance A.ToJSON PursuitEntry where
  toJSON (PursuitEntry name mdl detail mLibName) = A.object pairs
    where
    pairs = [ "name"   .= name
            , "module" .= mdl
            , "detail" .= detail
            ] ++ maybe [] (\x -> [ "library" .= x ]) mLibName

data PursuitDatabase =
  PursuitDatabase (M.Map String LibraryInfo) [PursuitEntry]

instance A.FromJSON PursuitDatabase where
  parseJSON (A.Object o) =
    PursuitDatabase <$> o .: "libraries" <*> o .: "entries"
  parseJSON val = fail $ "couldn't parse " ++ show val ++ " as PursuitDatabase"

instance A.ToJSON PursuitDatabase where
  toJSON (PursuitDatabase libs entries) = A.object pairs
    where
    pairs = [ "libraries" .= libs
            , "entries" .= entries
            ]

instance Monoid PursuitDatabase where
  mempty = PursuitDatabase mempty mempty
  mappend (PursuitDatabase aLibs aEntries) (PursuitDatabase bLibs bEntries) =
    PursuitDatabase (aLibs <> bLibs) (aEntries <> bEntries)
