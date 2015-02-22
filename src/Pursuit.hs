{-# LANGUAGE OverloadedStrings #-}

module Pursuit where

import Data.Monoid

import Data.Aeson ((.=), (.:), (.:?))
import qualified Data.Aeson as A

import qualified Data.Map as M

import Control.Applicative

type GitUrl = String

-- A Locator describes where to find a particular library.
data Locator =
  OnGithub String String
  deriving (Show, Eq)

toGitUrl :: Locator -> GitUrl
toGitUrl (OnGithub user repo) =
  "https://github.com/" ++ user ++ "/" ++ repo

instance A.FromJSON Locator where
  parseJSON (A.Object o) =
    OnGithub <$> o .: "user"
             <*> o .: "repo"
  parseJSON val = fail $ "couldn't parse " ++ show val ++ " as Locator"

-- The structure of one entry in the input libraries.json file.
data Library = Library { libraryLocator :: Locator
                       , libraryBowerName :: Maybe String
                       }
                       deriving (Show, Eq)

libraryGitUrl :: Library -> GitUrl
libraryGitUrl = toGitUrl . libraryLocator

-- This implementation will need to change if we decide to support libraries
-- which are not on github.
libraryWebUrl :: Library -> String
libraryWebUrl = libraryGitUrl

instance A.FromJSON Library where
  parseJSON (A.Object o) =
    Library <$> o .: "github"
            <*> o .:? "bowerName"
  parseJSON val = fail $ "couldn't parse " ++ show val ++ " as Library"

-- The data associated with a named library in the output.
data LibraryInfo = LibraryInfo { infoVersion :: String
                               , infoWebUrl  :: String
                               }

instance A.FromJSON LibraryInfo where
  parseJSON (A.Object o) =
    LibraryInfo <$> o .: "version"
                <*> o .: "webUrl"
  parseJSON val = fail $ "couldn't parse " ++ show val ++ " as LibraryInfo"

instance A.ToJSON LibraryInfo where
  toJSON (LibraryInfo vers webUrl) =
    A.object [ "version" .= vers
             , "webUrl"  .= webUrl
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
