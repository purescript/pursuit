-----------------------------------------------------------------------------
--
-- Module      :  Pursuit.Generator
-- Copyright   :  (c) Phil Freeman 2014
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- | Data generator for the pursuit search engine
--
-- TODO
-- ====
--
-- Just `git pull` rather than deleting and downloading the whole repo again
-- (but check this will work)
--
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}

module Pursuit.Generator (
  Error(..),
  Warning(..),
  PackageError(..),
  generateDatabase
) where

import Prelude hiding (log, mod)

import Data.List
import qualified Data.DList as DL
import Data.Ord
import Data.Maybe
import Data.Monoid
import Data.Version (parseVersion, Version(..))
import Text.ParserCombinators.ReadP (readP_to_S)

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Writer.Class
import Control.Monad.Writer
import Control.Monad.Except (ExceptT, runExceptT, MonadError, throwError)

import Pursuit.Data
import Pursuit.Docs
import Pursuit.Database

import System.Exit (ExitCode(..))
import System.Directory (getCurrentDirectory, setCurrentDirectory,
                         doesDirectoryExist, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import System.FilePath.Glob (glob)

import qualified Data.ByteString as B

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL

import qualified Data.Aeson as A

import qualified Text.Parsec as Parsec

import qualified Language.PureScript as P

-- A condition that means that the generator is unable to continue.
data Error
  = DecodeLibrariesFailed T.Text
  | CommandFailed Int T.Text T.Text
  | PreludeFailed PackageError
  deriving (Show)

-- A condition that indicates that something is wrong and should probably be
-- fixed, but is not severe enough that the generator is unable to continue.
data Warning
  = PackageFailed PackageName PackageError
  deriving (Show)

-- A condition that means that database entries for a particular package could
-- not be created.
data PackageError
  = NoSuitableTagsFound
  | ParseFailed Parsec.ParseError
  deriving (Show)

data LogMessage
  = CloningRepo GitUrl
  | SelectedVersion PackageName Version

data GenerateWriter =
  GenerateWriter (DL.DList Warning) (DL.DList LogMessage)

instance Monoid GenerateWriter where
  mempty = GenerateWriter mempty mempty
  mappend (GenerateWriter a b) (GenerateWriter c d) =
    GenerateWriter (mappend a c) (mappend b d)

unpackGenerateWriter :: GenerateWriter -> ([Warning], [LogMessage])
unpackGenerateWriter (GenerateWriter warns msgs) =
  (DL.toList warns, DL.toList msgs)

newtype Generate a =
  G { unG :: ExceptT Error (WriterT GenerateWriter IO) a }
  deriving (Functor, Applicative, Monad, MonadWriter GenerateWriter,
            MonadIO, MonadError Error)

runGenerate :: Generate a -> IO ([Warning], [LogMessage], Either Error a)
runGenerate action =
  fmap shuffle (runWriterT (runExceptT (unG action)))
  where
  shuffle (result, generateWriter) =
    let (warns, logs) = unpackGenerateWriter generateWriter
    in (warns, logs, result)

warn :: Warning -> Generate ()
warn w = tell (GenerateWriter (DL.singleton w) DL.empty)

log :: LogMessage -> Generate ()
log m = tell (GenerateWriter DL.empty (DL.singleton m))

-- A Module except that it doesn't have a Package yet.
type Module' = ModuleName

-- A Decl except that it doesn't have a Package or a Module yet.
type Decl' = (DeclName, DeclDetail)

completeModule' :: PackageName -> Module' -> Module
completeModule' pkgName modName =
  Module { moduleName        = modName
         , modulePackageName = pkgName
         }

completeDecl' :: ModuleName -> PackageName -> Decl' -> Decl
completeDecl' modName pkgName (declName, declDetail) =
  Decl { declName = declName
       , declDetail = declDetail
       , declModule = (modName, pkgName)
       }

completeAll :: PackageName -> (Module', [Decl']) -> (Module, [Decl])
completeAll pkgName (mod', decls') = (mod, decls)
  where
  mod   = completeModule' pkgName mod'
  decls = map (completeDecl' (moduleName mod) pkgName) decls'

getBaseDir :: IO FilePath
getBaseDir = do
  currentDir <- getCurrentDirectory
  return $ currentDir </> workingDir

generateDatabase ::
  FilePath -> IO ([Warning], [LogMessage], Either Error PursuitDatabase)
generateDatabase =
  runGenerate . generateDatabase'

generateDatabase' :: FilePath -> Generate PursuitDatabase
generateDatabase' packagesFile = do
  packages <- getPackageDescs packagesFile

  dbs <- getPackageDbs packages
  preludeDb <- buildPreludeDb

  return $ preludeDb <> mconcat dbs

getPackageDbs :: [PackageDesc] -> Generate [PursuitDatabase]
getPackageDbs packageDescs = do
  baseDir <- liftIO getBaseDir

  forM packageDescs $ \pkgDesc -> do
    let name = packageDescName pkgDesc
    let dir = baseDir </> T.unpack (runPackageName name)

    gitClone (packageDescGitUrl pkgDesc) dir
    mVers <- getMostRecentTaggedVersion dir
    case mVers of
      Nothing ->
        failPackage name NoSuitableTagsFound
      Just (version, versionStr) -> do
        gitCheckoutTag versionStr dir
        log (SelectedVersion name version)

        let pkg = buildPackage pkgDesc version
        buildPackageDb pkg dir

buildPackage :: PackageDesc -> Version -> Package
buildPackage (PackageDesc{..}) version =
  Package { packageName = packageDescName
          , packageLocator = packageDescLocator
          , packageVersion = version
          }

failPackage :: PackageName -> PackageError -> Generate PursuitDatabase
failPackage pkgName reason = do
  warn (PackageFailed pkgName reason)
  return mempty

workingDir :: String
workingDir = "./tmp/"

getPackageDescs :: FilePath -> Generate [PackageDesc]
getPackageDescs packagesFile = do
  json <- liftIO $ B.readFile packagesFile
  case A.eitherDecodeStrict json of
    Right libs -> return libs
    Left err -> throwError (DecodeLibrariesFailed (T.pack err))

-- Clone the specified repository into the specified directory.
gitClone :: GitUrl -> FilePath -> Generate ()
gitClone url dir = do
  liftIO $ do
    exists <- doesDirectoryExist dir
    when exists $
      removeDirectoryRecursive dir

  log (CloningRepo url)
  runCommandQuiet "git" ["clone", url, dir]

gitCheckoutTag :: String -> FilePath -> Generate ()
gitCheckoutTag tag gitDir = do
  pushd gitDir $ do
    runCommandQuiet "git" ["checkout", tag]

getMostRecentTaggedVersion :: FilePath -> Generate (Maybe (Version, String))
getMostRecentTaggedVersion gitDir = do
  pushd gitDir $ do
    (out, _) <- runCommand "git" ["tag", "--list"]
    let versions = mapMaybe parseVersion' (lines out)
    return (listToMaybe (reversedSort versions))

pushd :: (Monad m, MonadIO m) => FilePath -> m a -> m a
pushd dir action = do
  oldDir <- liftIO $ getCurrentDirectory
  liftIO $ setCurrentDirectory dir
  result <- action
  liftIO $ setCurrentDirectory oldDir
  return result

parseVersion' :: String -> Maybe (Version, String)
parseVersion' ('v':xs) = fmap prependV $ parseVersion' xs
  where prependV (vers, str) = (vers, 'v' : str)
parseVersion' str =
  case filter (null . snd) $ readP_to_S parseVersion str of
    [(vers, "")] -> Just (vers, str)
    _ -> Nothing

reversedSort :: Ord a => [a] -> [a]
reversedSort = sortBy (comparing Down)

runCommand :: FilePath -> [String] -> Generate (String, String)
runCommand program args = do
  (code, out, err) <- liftIO $ readProcessWithExitCode program args ""
  case code of
    ExitSuccess ->
      return (out, err)
    ExitFailure n ->
      throwError (CommandFailed n (T.pack out) (T.pack err))

runCommandQuiet :: FilePath -> [String] -> Generate ()
runCommandQuiet program args =
  void $ runCommand program args

-- Build a subset of the full database for a single package, which may or may
-- not have a name or associated information. In all cases, the exported types
-- and values appear in the database. If both a package name and additional
-- package information are supplied, then those appear in the database too.
buildPackageDb :: Package ->  FilePath -> Generate PursuitDatabase
buildPackageDb pkg dir = do
  declsFromDir dir >>= \case
    Left err ->
      failPackage (packageName pkg) (ParseFailed err)
    Right decls' ->
      let databases = map (databaseFromDecls pkg) decls'
      in return (mconcat databases)

-- Build a PursuitDatabase from a list of entries, optionally also with details
-- of the package they come from.
databaseFromDecls :: Package -> (Module', [Decl']) -> PursuitDatabase
databaseFromDecls pkg (mod', decls') =
  createDatabase [pkg] [mod]
                 (map (completeDecl' (moduleName mod) (packageName pkg)) decls')
  where
  mod = completeModule' (packageName pkg) mod'

declsFromDir ::
  FilePath -> Generate (Either Parsec.ParseError [(Module', [Decl'])])
declsFromDir dir = do
  files <- liftIO $ glob $ dir </> "src/**/*.purs"
  parsedFiles <- mapM parseFile files

  return (modulesToDecls . concat <$> sequence parsedFiles)

buildPreludeDb :: Generate PursuitDatabase
buildPreludeDb = do
  modules <- parseText "<<Prelude>>" (T.pack P.prelude) >>= \case
    Left err -> throwError (PreludeFailed (ParseFailed err))
    Right ms -> return ms

  let (mods, decls) = getModulesAndDecls (packageName preludePkg) modules
  return (createDatabase [preludePkg] mods decls)
  where
  -- TODO: Use the actual version of PureScript
  preludePkg = Package { packageName    = PackageName "prelude"
                       , packageLocator = BundledWithCompiler
                       , packageVersion = Version [0,6,8] []
                       }

getModulesAndDecls :: PackageName -> [P.Module] -> ([Module], [Decl])
getModulesAndDecls pkgName =
  shuffle . map (completeAll pkgName) . modulesToDecls
  where
  shuffle = foldr (\(mod, decls) (mods, decls') ->
                      (mod : mods, decls ++ decls'))
                  ([], [])

modulesToDecls :: [P.Module] -> [(Module', [Decl'])]
modulesToDecls = map declsForModule

parseFile :: FilePath -> Generate (Either Parsec.ParseError [P.Module])
parseFile input = do
  text <- liftIO $ T.readFile input
  parseText input text

parseText ::
  FilePath -> T.Text -> Generate (Either Parsec.ParseError [P.Module])
parseText input text =
  return (P.lex input (T.unpack text) >>= P.runTokenParser input P.parseModules)

declsForModule :: P.Module -> (Module', [Decl'])
declsForModule mod@(P.Module _ ds exps) =
  (toModule' mod, concatMap (toDecls' exps) ds)

toModule' :: P.Module -> Module'
toModule' (P.Module mn _ _) = ModuleName (T.pack (show mn))

toDecls' :: Maybe [P.DeclarationRef] -> P.Declaration -> [Decl']
toDecls' exps = go
  where
  go d = case getName d of
           Just name -> makeDecl name (declarationDocs exps d) : concatMap go (relatedDecls d)
           _ -> []
  
  getName :: P.Declaration -> Maybe String
  getName (P.TypeDeclaration name _)                = Just (show name)
  getName (P.ExternDeclaration _ name _ _)          = Just (show name)
  getName (P.DataDeclaration _ name _ _)            = Just (show name)
  getName (P.ExternDataDeclaration name _)          = Just (show name)
  getName (P.TypeSynonymDeclaration name _ _)       = Just (show name)
  getName (P.TypeClassDeclaration name _ _ _)       = Just (show name)
  getName (P.TypeInstanceDeclaration name _ _ _ _)  = Just (show name)
  getName (P.PositionedDeclaration _ _ d)           = getName d
  getName _                                         = Nothing
  
  relatedDecls :: P.Declaration -> [P.Declaration]
  relatedDecls (P.TypeClassDeclaration _ _ _ ds)         = ds
  relatedDecls (P.PositionedDeclaration _ _ d)           = relatedDecls d
  relatedDecls _                                         = []
  
  makeDecl :: String -> TL.Text -> Decl'
  makeDecl name detail = (DeclName (T.pack name), DeclDetail detail)
  
