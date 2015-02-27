module Pursuit.Database where

import Data.Monoid

import Data.IxSet

import Pursuit.Data

data PursuitDatabase =
  PursuitDatabase { dbPackages :: IxSet Package
                  , dbModules  :: IxSet Module
                  , dbDecls    :: IxSet Decl
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
    PursuitDatabase (mappend a1 a2)
                    (mappend b1 b2)
                    (mappend c1 c2)

queryDecls :: String -> PursuitDatabase -> [Decl]
queryDecls q db = []
