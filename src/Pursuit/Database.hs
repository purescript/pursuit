{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Pursuit.Database (
  PursuitDatabase(),
  createDatabase,

  Query(),
  runQuery,

  queryDecls,
  queryDeclsJ,
  getModuleByPK,
  getPackageByPK,
  getModulesByPackage,
  getDeclsByModule,
) where

import Prelude hiding (mod)

import Data.Monoid
import Data.Typeable
import Data.Maybe
import Data.IxSet hiding ((&&&))
import qualified Data.Text as T

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Trans.Maybe

import Pursuit.Data

data PursuitDatabase =
  PursuitDatabase { dbPackages :: IxSet Package
                  , dbModules  :: IxSet Module
                  , dbDecls    :: IxSet Decl
                  }

createDatabase :: [Package] -> [Module] -> [Decl] -> PursuitDatabase
createDatabase pkgs mods decls =
  PursuitDatabase (fromList pkgs)
                  (fromList mods)
                  (fromList decls)

instance Monoid PursuitDatabase where
  mempty = PursuitDatabase mempty mempty mempty
  mappend (PursuitDatabase a1 b1 c1) (PursuitDatabase a2 b2 c2) =
    PursuitDatabase (mappend a1 a2)
                    (mappend b1 b2)
                    (mappend c1 c2)

newtype Query a = Query { unQuery :: Reader PursuitDatabase a }
  deriving (Functor, Applicative, Monad, MonadReader PursuitDatabase)

runQuery :: Query a -> PursuitDatabase -> a
runQuery = runReader . unQuery

queryOne :: (Indexable a, Typeable k, Typeable a, Ord a) =>
  (PursuitDatabase -> IxSet a) -> k -> Query (Maybe a)
queryOne dbPart key = go <$> asks dbPart
  where
  go xs = getOne (xs @= key)

queryAll :: (Indexable a, Typeable k, Typeable a, Ord a) =>
  (PursuitDatabase -> IxSet a) -> k -> Query [a]
queryAll dbPart key = go <$> asks dbPart
  where
  go xs = toList (xs @= key)

-- Search for declarations with names matching the query.
queryDecls :: T.Text -> Query [Decl]
queryDecls q' = go <$> asks dbDecls
  where
  q = stripParens (T.toLower q')
  go decls = toList (decls @+ [DeclName q, DeclName ("(" <> q <> ")")])

  stripParens' = T.stripPrefix "(" >=> T.stripSuffix ")"
  stripParens t = fromMaybe t (stripParens' t)

getModuleByPK :: (ModuleName, PackageName) -> Query (Maybe Module)
getModuleByPK = queryOne dbModules

getPackageByPK :: PackageName -> Query (Maybe Package)
getPackageByPK = queryOne dbPackages

queryDeclsJ :: T.Text -> Query [DeclJ]
queryDeclsJ q = catMaybes <$> (queryDecls q >>= mapM joinDecl)

joinDecl :: Decl -> Query (Maybe DeclJ)
joinDecl d = runMaybeT $ do
  mod     <- MaybeT (getModuleByPK (declModule d))
  package <- MaybeT (getPackageByPK (modulePackageName mod))
  return (d,mod,package)

getModulesByPackage :: PackageName -> Query [Module]
getModulesByPackage = queryAll dbModules

getDeclsByModule :: (ModuleName, PackageName) -> Query [Decl]
getDeclsByModule = queryAll dbDecls
