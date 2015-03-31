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
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

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
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Class (tell, MonadWriter)
import Control.Monad.Reader.Class (asks, MonadReader)
import Control.Monad.Writer (runWriterT, WriterT)
import Control.Monad.Reader (runReaderT, ReaderT)
import Control.Monad.Except (ExceptT, runExceptT, MonadError, throwError)
import Control.Exception (try, IOException)

import Pursuit.Data
import Pursuit.Docs (itemDocs)
import Pursuit.Database
import Pursuit.Prim (primModule)

import System.Exit (ExitCode(..))
import System.Directory (getCurrentDirectory, setCurrentDirectory,
                         doesDirectoryExist, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import System.FilePath.Glob (glob)

import qualified Data.ByteString as B

import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as TL

import qualified Data.Aeson as A

import qualified Text.Parsec as Parsec

import Github.Repos (tagsFor', tagName)
import Github.Auth (GithubAuth)
import qualified Github.Data.Definitions as Github

import qualified Language.PureScript as P

-- A condition that means that the generator is unable to continue.
data Error
  = DecodeLibrariesFailed T.Text
  | CommandFailed Int T.Text T.Text
  | PreludeFailed PackageError
  | GithubError Github.Error
  | IOExceptionThrown IOException
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
  deriving (Show)

data GenerateWriter =
  GenerateWriter (DL.DList Warning) (DL.DList LogMessage)

instance Monoid GenerateWriter where
  mempty = GenerateWriter mempty mempty
  mappend (GenerateWriter a b) (GenerateWriter c d) =
    GenerateWriter (mappend a c) (mappend b d)

data GenerateReader =
  GenerateReader { readerAuth :: Maybe GithubAuth
                 }

unpackGenerateWriter :: GenerateWriter -> ([Warning], [LogMessage])
unpackGenerateWriter (GenerateWriter warns msgs) =
  (DL.toList warns, DL.toList msgs)

type Result a = ([Warning], [LogMessage], Either Error a)

newtype Generate a =
  G { unG :: ExceptT Error (WriterT GenerateWriter (ReaderT GenerateReader IO)) a }
  deriving (Functor, Applicative, Monad, MonadWriter GenerateWriter,
            MonadReader GenerateReader, MonadError Error)

-- | Lift an IO action into the Generate monad. If an IOException occurs, it is
-- handled by the ExceptT part of the Generate stack.
io :: IO a -> Generate a
io act =
  liftIO (try act) >>= \case
    Left exc -> throwError (IOExceptionThrown exc)
    Right x  -> return x
  where
  liftIO :: IO a -> Generate a
  liftIO = G . lift . lift . lift

-- | Lift a GitHub action into the Generate monad.
github :: IO (Either Github.Error a) -> Generate a
github act =
  io act >>= \case
    Left err -> throwError (GithubError err)
    Right x  -> return x

runGenerate :: Generate a -> GenerateReader -> IO (Result a)
runGenerate action rdr =
  fmap shuffle (runReaderT (runWriterT (runExceptT (unG action))) rdr)
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

generateDatabase :: FilePath -> Maybe GithubAuth -> IO (Result PursuitDatabase)
generateDatabase path auth =
  runGenerate (generateDatabase' path) (GenerateReader auth)

generateDatabase' :: FilePath -> Generate PursuitDatabase
generateDatabase' packagesFile = do
  packages <- getPackageDescs packagesFile

  dbs <- getPackageDbs packages
  preludeDb <- buildPreludeDb

  return $ preludeDb <> mconcat dbs

getPackageDbs :: [PackageDesc] -> Generate [PursuitDatabase]
getPackageDbs packageDescs = do
  baseDir <- io getBaseDir

  forM packageDescs $ \pkgDesc -> do
    let name = packageDescName pkgDesc
    let dir = baseDir </> T.unpack (runPackageName name)

    gitClone (packageDescGitUrl pkgDesc) dir
    mVers <- getMostRecentTaggedVersion pkgDesc
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
  json <- io $ B.readFile packagesFile
  case A.eitherDecodeStrict json of
    Right libs -> return libs
    Left err -> throwError (DecodeLibrariesFailed (T.pack err))

-- Clone the specified repository into the specified directory.
gitClone :: GitUrl -> FilePath -> Generate ()
gitClone url dir = do
  io $ do
    exists <- doesDirectoryExist dir
    when exists $
      removeDirectoryRecursive dir

  log (CloningRepo url)
  runCommandQuiet "git" ["clone", url, dir]

gitCheckoutTag :: String -> FilePath -> Generate ()
gitCheckoutTag tag gitDir = do
  pushd gitDir $ do
    runCommandQuiet "git" ["checkout", tag]

getMostRecentTaggedVersion :: PackageDesc -> Generate (Maybe (Version, String))
getMostRecentTaggedVersion pkgDesc = do
  auth <- asks readerAuth
  tags <- map tagName <$> github (tagsFor' auth owner repo)
  let versions = mapMaybe parseVersion' tags
  return (listToMaybe (reversedSort versions))
  where
  OnGithub owner repo = packageDescLocator pkgDesc

pushd :: FilePath -> Generate a -> Generate a
pushd dir action = do
  oldDir <- io $ getCurrentDirectory
  io $ setCurrentDirectory dir
  result <- action
  io $ setCurrentDirectory oldDir
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
  (code, out, err) <- io $ readProcessWithExitCode program args ""
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
  files <- io $ glob $ dir </> "src/**/*.purs"
  parsedFiles <- mapM parseFile files

  return (modulesToDecls . concat <$> sequence parsedFiles)

buildPreludeDb :: Generate PursuitDatabase
buildPreludeDb = do
  preludeModules <- parseModules "<<Prelude>>" (T.pack P.prelude)
  primModules    <- parseModules "<<Prim>>" primModule

  let (mods, decls) = getModulesAndDecls (packageName basePkg)
                                         (preludeModules <> primModules)

  return (createDatabase [basePkg] mods decls)
  where
  parseModules name sourceText =
      parseText name sourceText >>= \case
        Left err -> throwError (PreludeFailed (ParseFailed err))
        Right ms -> return ms

  basePkg = Package { packageName    = PackageName "purescript"
                    , packageLocator = BundledWithCompiler
                    , packageVersion = P.version
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

readFileText :: FilePath -> IO T.Text
readFileText = fmap decodeUtf8 . B.readFile

parseFile :: FilePath -> Generate (Either Parsec.ParseError [P.Module])
parseFile input = do
  text <- io $ readFileText input
  parseText input text

parseText ::
  FilePath -> T.Text -> Generate (Either Parsec.ParseError [P.Module])
parseText input text =
  return (P.lex input (T.unpack text) >>= P.runTokenParser input P.parseModules)

declsForModule :: P.Module -> (Module', [Decl'])
declsForModule m =
  (toModule' m, concatMap (toDecls' m) (P.exportedDeclarations m))

toModule' :: P.Module -> Module'
toModule' = ModuleName . T.pack . show . P.getModuleName

toDecls' :: P.Module -> P.Declaration -> [Decl']
toDecls' mod = go . ItemDecl
  where
  go d =
    case getName d of
      Just name -> let mDecl = makeDecl name (itemDocs mod d)
                       rest = concatMap go (relatedItems d)
                   in maybe id (:) mDecl rest
      _ -> []

  getName :: Item -> Maybe String
  getName (ItemDecl d)     = getDeclName d
  getName (ItemDataCtor c) = getDataCtorName c

  getDeclName :: P.Declaration -> Maybe String
  getDeclName (P.TypeDeclaration name _)                = Just (show name)
  getDeclName (P.ExternDeclaration _ name _ _)          = Just (show name)
  getDeclName (P.DataDeclaration _ name _ _)            = Just (show name)
  getDeclName (P.ExternDataDeclaration name _)          = Just (show name)
  getDeclName (P.TypeSynonymDeclaration name _ _)       = Just (show name)
  getDeclName (P.TypeClassDeclaration name _ _ _)       = Just (show name)
  getDeclName (P.TypeInstanceDeclaration name _ _ _ _)  = Just (show name)
  getDeclName (P.PositionedDeclaration _ _ d)           = getDeclName d
  getDeclName _                                         = Nothing

  getDataCtorName :: (P.ProperName, P.ProperName, [P.Type]) -> Maybe String
  getDataCtorName (_, n, _) = Just (P.runProperName n)

  relatedItems :: Item -> [Item]
  relatedItems (ItemDecl d)     = relatedItems' d
  relatedItems (ItemDataCtor _) = []

  relatedItems' :: P.Declaration -> [Item]
  relatedItems' (P.TypeClassDeclaration _ _ _ ds) = ItemDecl <$> ds
  relatedItems' (P.PositionedDeclaration _ _ d)   = relatedItems' d
  relatedItems' (P.DataDeclaration _ ty _ cs)     = (\(c, as) -> ItemDataCtor (ty, c, as)) <$> cs
  relatedItems' _                                 = []

makeDecl :: String -> Maybe TL.Text -> Maybe Decl'
makeDecl name mDetail = go' <$> mDetail
  where
  go' detail = (DeclName (T.pack name), DeclDetail detail)
