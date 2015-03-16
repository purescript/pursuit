{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Pursuit.Data (
  PackageDesc(..),
  Package(..),
  Module(..),
  Decl(..),
  GitUrl(),

  DeclJ(), ModuleJ(),

  PackageName(..), runPackageName, withPackageName,
  ModuleName(..),  runModuleName,  withModuleName,
  DeclName(..),    runDeclName,    withDeclName,

  Locator(..),
  DeclDetail(..), runDeclDetail,

  Item(..),

  preludeWebUrl,
  packageDescGitUrl,
  packageWebUrl
) where

import Prelude hiding (mod)

import Data.Version
import Data.Typeable
import Data.IxSet hiding ((&&&))

import Data.Aeson ((.:))
import qualified Data.Aeson as A

import qualified Data.Text.Lazy as TL
import qualified Data.Text as T

import qualified Lucid as L

import Control.Arrow
import Control.Applicative

import qualified Language.PureScript as P

-- This is what appears in the packages.json file
data PackageDesc = PackageDesc { packageDescName    :: PackageName
                               , packageDescLocator :: Locator
                               }
                               deriving (Show)

instance A.FromJSON PackageDesc where
  parseJSON (A.Object o) =
    PackageDesc <$> o .: "bowerName"
                <*> o .: "github"
  parseJSON val = fail $ "couldn't parse " ++ show val ++ " as PackageDesc"

newtype PackageName = PackageName T.Text
  deriving (Show, Eq, Ord, Typeable, A.FromJSON, L.ToHtml)

withPackageName :: (T.Text -> T.Text) -> PackageName -> PackageName
withPackageName f (PackageName str) = PackageName (f str)

runPackageName :: PackageName -> T.Text
runPackageName (PackageName n) = n

-- A Locator describes where to find a particular package.
data Locator
  = OnGithub String String
  | BundledWithCompiler
  deriving (Show, Eq, Ord)

preludeWebUrl :: String
preludeWebUrl = "https://github.com/purescript/purescript/tree/master/prelude"

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

packageDescGitUrl :: PackageDesc -> GitUrl
packageDescGitUrl = toGitUrl . packageDescLocator

-- A Package has a name (primary key), a Locator, and a Version.
data Package = Package { packageName    :: PackageName
                       , packageLocator :: Locator
                       , packageVersion :: Version
                       }
                       deriving (Show, Eq, Ord, Typeable)

-- This will need to change if we decide to support packages which are not
-- hosted on GitHub.
packageWebUrl :: Package -> T.Text
packageWebUrl = T.pack . toGitUrl . packageLocator

-- A Module belongs to exactly one Package. The primary key is composite:
-- (moduleName, modulePackageName)
data Module = Module { moduleName        :: ModuleName
                     , modulePackageName :: PackageName
                     }
                     deriving (Show, Eq, Ord, Typeable)

newtype ModuleName = ModuleName T.Text
  deriving (Show, Eq, Ord, Typeable, L.ToHtml)

runModuleName :: ModuleName -> T.Text
runModuleName (ModuleName n) = n

withModuleName :: (T.Text -> T.Text) -> ModuleName -> ModuleName
withModuleName f = ModuleName . f . runModuleName

-- A Decl belongs to exactly one Module. The primary key is composite:
-- (declName, declModule)
data Decl = Decl { declName   :: DeclName
                 , declDetail :: DeclDetail
                 , declModule :: (ModuleName, PackageName)
                 }
                 deriving (Show, Eq, Ord, Typeable)

newtype DeclName = DeclName T.Text
  deriving (Show, Eq, Ord, Typeable, L.ToHtml)

runDeclName :: DeclName -> T.Text
runDeclName (DeclName n) = n

withDeclName :: (T.Text -> T.Text) -> DeclName -> DeclName
withDeclName f (DeclName str) = DeclName (f str)

newtype DeclDetail = DeclDetail TL.Text
  deriving (Show, Eq, Ord, Typeable, L.ToHtml)

runDeclDetail :: DeclDetail -> TL.Text
runDeclDetail (DeclDetail d) = d

-- | An item to be included in the docs.
data Item
  = ItemDecl P.Declaration
  -- | A data constructor, with a type name, constructor name, and constructor
  -- arguments.
  | ItemDataCtor (P.ProperName, P.ProperName, [P.Type])

singleton :: a -> [a]
singleton = (:[])

instance Indexable Package where
  empty = ixSet [ ixFun (singleton . packageName) ]

instance Indexable Module where
  empty = ixSet [ ixFun (singleton . (moduleName &&& modulePackageName))
                , ixFun (singleton . moduleName)
                , ixFun (singleton . modulePackageName)
                ]

instance Indexable Decl where
  empty = ixSet [ ixFun (\d -> let (mod, pkg) = declModule d
                               in singleton (declName d, mod, pkg))
                , ixFun (singleton . withDeclName (T.toLower) . declName)
                , ixFun (singleton . declDetail)
                ]

-- | "Decl-joined"; a Declaration, together with its parent Module and Package.
type DeclJ = (Decl, Module, Package)

-- | "Module-joined"; a Module, together with its parent Package.
type ModuleJ = (Module, Package)
