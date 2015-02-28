module Pursuit.Database where

import Data.Char (toLower)
import Data.Monoid
import Data.IxSet hiding ((&&&))

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

-- Search for declarations with names matching the query.
queryDecls :: String -> PursuitDatabase -> [Decl]
queryDecls q db = toList (dbDecls db @= DeclName (map toLower q))
