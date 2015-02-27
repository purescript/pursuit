{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pursuit where

import Data.Monoid
import Data.Version

import Data.Aeson ((.:))
import qualified Data.Aeson as A

import qualified Data.Map as M

import Control.Applicative

-- This is what appears in the packages.json file
data PackageDesc = PackageDesc { packageDescName    :: PackageName
                               , packageDescLocator :: Locator
                               }
                               deriving (Show)

instance A.FromJSON PackageDesc where
  parseJSON (A.Object o) =
    PackageDesc <$> o .: "name"
                <*> o .: "github"
  parseJSON val = fail $ "couldn't parse " ++ show val ++ " as PackageDesc"

newtype PackageName = PackageName String
  deriving (Show, Eq, Ord, A.FromJSON)

runPackageName :: PackageName -> String
runPackageName (PackageName n) = n

-- A Locator describes where to find a particular package.
data Locator
  = OnGithub String String
  | BundledWithCompiler
  deriving (Show, Eq)

type GitUrl = String

toGitUrl :: Locator -> GitUrl
toGitUrl (OnGithub user repo) =
  "https://github.com/" ++ user ++ "/" ++ repo
toGitUrl BundledWithCompiler =
  "https://github.com/purescript/purescript"

instance A.FromJSON Locator where
  parseJSON (A.Object o) =
    OnGithub <$> o .: "user"
             <*> o .: "repo"
  parseJSON val = fail $ "couldn't parse " ++ show val ++ " as Locator"

-- A Package has a name (primary key), a Locator, and a Version.
data Package = Package { packageName    :: PackageName
                       , packageLocator :: Locator
                       , packageVersion :: Version
                       }

packageDescGitUrl :: PackageDesc -> GitUrl
packageDescGitUrl = toGitUrl . packageDescLocator

-- This will need to change if we decide to support packages which are not
-- hosted on GitHub.
packageWebUrl :: Package -> String
packageWebUrl = toGitUrl . packageLocator


-- A Module belongs to exactly one Package. The primary key is composite:
-- (moduleName, modulePackageName)
data Module = Module { moduleName        :: ModuleName
                     , modulePackageName :: PackageName
                     }

newtype ModuleName = ModuleName String
  deriving (Show, Eq, Ord)

-- A Decl belongs to exactly one Module. The primary key is composite:
-- (declName, declModule)
data Decl = Decl { declName   :: DeclName
                 , declDetail :: DeclDetail
                 , declModule :: (ModuleName, PackageName)
                 }

newtype DeclName = DeclName String
  deriving (Show, Eq, Ord)
newtype DeclDetail = DeclDetail String
  deriving (Show, Eq, Ord)

data PursuitDatabase =
  PursuitDatabase { dbPackages :: M.Map PackageName Package
                  , dbModules  :: M.Map (ModuleName, PackageName) Module
                  , dbDecls    :: M.Map (DeclName, ModuleName, PackageName) Decl
                  }

createDatabase :: [Package] -> [Module] -> [Decl] -> PursuitDatabase
createDatabase pkgs mods decls =
  PursuitDatabase (makeMap packageName pkgs)
                  (makeMap (\m -> (moduleName m, modulePackageName m)) mods)
                  (makeMap (\d -> let m = declModule d
                                  in (declName d, fst m, snd m)) decls)
  where
  makeMap key = foldr (\x -> M.insert (key x) x) M.empty

instance Monoid PursuitDatabase where
  mempty = PursuitDatabase mempty mempty mempty
  mappend (PursuitDatabase a1 b1 c1) (PursuitDatabase a2 b2 c2) =
    PursuitDatabase (a1 <> a2) (b1 <> b2) (c1 <> c2)
