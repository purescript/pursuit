{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Pursuit.Database (
  PursuitDatabase(),
  createDatabase,

  Query(),
  runQuery,

  queryDecls,
  getModuleByPK,
  getPackageByPK,
  queryDeclsJ
) where

import Prelude hiding (mod)

import Data.Monoid
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

-- Search for declarations with names matching the query.
queryDecls :: T.Text -> Query [Decl]
queryDecls q' = go <$> asks dbDecls
  where
  q = stripParens (T.toLower q')
  go decls = toList (decls @+ [DeclName q, DeclName ("(" <> q <> ")")])

  stripParens' = T.stripPrefix "(" >=> T.stripSuffix ")"
  stripParens t = fromMaybe t (stripParens' t)

getModuleByPK :: (ModuleName, PackageName) -> Query (Maybe Module)
getModuleByPK key = go <$> asks dbModules
  where
  go modules = getOne (modules @= key)

getPackageByPK :: PackageName -> Query (Maybe Package)
getPackageByPK key = go <$> asks dbPackages
  where
  go packages = getOne (packages @= key)

queryDeclsJ :: T.Text -> Query [DeclJ]
queryDeclsJ q = catMaybes <$> (queryDecls q >>= mapM joinDecl)

joinDecl :: Decl -> Query (Maybe DeclJ)
joinDecl d = runMaybeT $ do
  mod     <- MaybeT (getModuleByPK (declModule d))
  package <- MaybeT (getPackageByPK (modulePackageName mod))
  return (d,mod,package)
