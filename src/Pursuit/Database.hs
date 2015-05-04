
module Pursuit.Database
  ( PursuitDatabase
  , lookupPackage
  , availableVersionsFor
  , insertPackage
  ) where

import Import.NoFoundation
import qualified Data.Map as M
import Data.Version (Version)

import qualified Web.Bower.PackageMeta as Bower
import qualified Language.PureScript.Docs as D

type PursuitDatabase_ = Map Bower.PackageName (Map Version D.VerifiedPackage)

newtype PursuitDatabase = PursuitDatabase PursuitDatabase_

runPursuitDatabase :: PursuitDatabase -> PursuitDatabase_
runPursuitDatabase (PursuitDatabase x) = x

overDb :: (PursuitDatabase_ -> PursuitDatabase_) -> PursuitDatabase -> PursuitDatabase
overDb f = PursuitDatabase . f . runPursuitDatabase

instance Monoid PursuitDatabase where
  mempty = PursuitDatabase mempty
  mappend (PursuitDatabase a) (PursuitDatabase b) =
    PursuitDatabase (M.unionWith M.union a b)

availableVersionsFor :: Bower.PackageName -> PursuitDatabase -> Maybe [Version]
availableVersionsFor pkgName = go . runPursuitDatabase
  where
  go db = M.keys <$> M.lookup pkgName db

lookupPackage :: Bower.PackageName -> Version -> PursuitDatabase -> Maybe D.VerifiedPackage
lookupPackage pkgName pkgVersion = go . runPursuitDatabase
  where
  go = M.lookup pkgName >=> M.lookup pkgVersion

-- | Insert a particular version of a package into a database. If a version of
-- that package at that version already exists, it is replaced.
insertPackage :: D.VerifiedPackage -> PursuitDatabase -> PursuitDatabase
insertPackage pkg@D.Package{..} =
  overDb (M.alter go (D.packageName pkg))
  where
  go = Just . M.insert pkgVersion pkg . fromMaybe M.empty
