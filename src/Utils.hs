{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Utils  
( loadDatabase
, getAllPackageNamesIO
, packageDirForIO
, lookupPackageIO
, availableVersionsForIO
, packageVersionFileForIO
, SomethingMissing(..)
, readFileMay
, catchDoesNotExist
) where

import Import
import qualified Language.PureScript.Docs as D
import qualified Data.Pool
import Data.ByteString (ByteString)
import Model
import qualified Data.Aeson as A
import Database.Persist.Sqlite (createSqlitePool, runSqlPool, sqlDatabase, sqlPoolSize, BaseBackend, SqlBackend, PersistQueryWrite, IsPersistBackend, Filter)
import Data.Version (showVersion)
import Data.Either.Extra (Either(..), fromRight')
import Data.Version (Version, showVersion)
import Web.Bower.PackageMeta (PackageName, mkPackageName, runPackageName, bowerDependencies, runVersionRange)
import System.Directory
  (getDirectoryContents, getModificationTime, doesDirectoryExist)

data SomethingMissing
  = NoSuchPackage
    | NoSuchPackageVersion
    deriving (Show, Eq, Ord)    

catchDoesNotExist :: IO a -> IO (Maybe a)
catchDoesNotExist act =
  catchJust selectDoesNotExist
          (Just <$> act)
          (const (return Nothing))
  where
    selectDoesNotExist e
      | isDoesNotExistErrorType (ioeGetErrorType e) = Just ()
      | otherwise = Nothing

-- | Read the file at the given path as a strict ByteString, or return Nothing
-- if no such file exists.
readFileMay :: FilePath -> IO (Maybe ByteString)
readFileMay file =
  catchDoesNotExist (readFile file)

packageDirForIO :: String -> PackageName -> IO String
packageDirForIO dir pkgName = do
  return (dir ++ "/verified/" ++ unpack (runPackageName pkgName))

packageVersionFileForIO :: String -> PackageName -> Version -> IO String
packageVersionFileForIO dataDir pkgName version = do
  dir <- packageDirForIO dataDir pkgName
  return (dir ++ "/" ++ showVersion version ++ ".json")

getAllPackageNamesIO :: String -> IO [PackageName]
getAllPackageNamesIO dir = do
  contents <- getDirectoryContents (dir ++ "/verified/")
  return . sort . rights $ map (mkPackageName . pack) contents

availableVersionsForIO :: String -> PackageName -> IO [Version]
availableVersionsForIO dataDir pkgName = do
  dir <- packageDirForIO dataDir pkgName
  mresult <- catchDoesNotExist $ do
      files <- getDirectoryContents dir
      return $ mapMaybe (stripSuffix ".json" >=> D.parseVersion') files
  return $ fromMaybe [] mresult

lookupPackageIO :: String -> PackageName -> Version -> IO (Either SomethingMissing D.VerifiedPackage)
lookupPackageIO dataDir pkgName version = do
  file <- packageVersionFileForIO dataDir pkgName version
  mcontents <- readFileMay file
  case mcontents of
      Just contents -> case A.eitherDecodeStrict contents of
          -- TODO: this should be a corect error
          Left _ -> return $ Left NoSuchPackage
          Right pkg -> return $ Right pkg
      Nothing -> do
          -- Work out whether there's no such package or just no such version
          dir <- packageDirForIO dataDir pkgName
          exists <- doesDirectoryExist dir
          return $ Left $ if exists then NoSuchPackageVersion else NoSuchPackage    

-- | This function loads all the dependencies into the sqlite database
loadDatabase :: 
  (BaseBackend backend ~ SqlBackend, PersistQueryWrite backend, IsPersistBackend backend) 
  => String -> Data.Pool.Pool backend -> IO ()
loadDatabase dir pool = do
  runSqlPool (deleteWhere ([] :: [Filter Package])) pool
  packageNames <- getAllPackageNamesIO dir
  forM_ packageNames $ \packageName -> do
    versions <- availableVersionsForIO dir packageName
    forM_ versions $ \version -> do
        eitherPackage <- lookupPackageIO dir packageName version
        case eitherPackage of
            -- TODO: Should I at least log the error value?
            Left _ -> return ()
            Right pkg -> forM_ (getDepList pkg) $ \(pkgDep, versionRange) -> do
                -- TODO: Should I do something with the return value?
                _ <-insertPkgDep 
                  (runPackageName packageName) 
                  (pack (showVersion version))
                  pkgDep
                  versionRange
                return ()
  where
    insertPkgDep pkg ver dep depver = runSqlPool (insert (Package pkg ver dep depver)) pool
    getDepList p = map (\(x,y) -> (runPackageName x,runVersionRange y)) $ bowerDependencies $ D.pkgMeta $ p

