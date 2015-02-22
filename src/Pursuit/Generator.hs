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
-- Convert commented out calls to 'p' to an actual logging system.
--
-- Just `git pull` rather than deleting and downloading the whole repo again
-- (but check this will work)
--
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

module Pursuit.Generator (
  Error(..),
  Warning(..),
  LibraryError(..),
  generateDatabase
) where

import Data.List
import qualified Data.DList as DL
import Data.Ord
import Data.List.Split (splitOn)
import Data.Maybe
import Data.Monoid
import Data.Version (showVersion, parseVersion, Version)
import Text.ParserCombinators.ReadP (readP_to_S)

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Writer.Class
import Control.Monad.Writer
import Control.Monad.Except (ExceptT, runExceptT, MonadError, throwError)

import Pursuit

import System.Exit (ExitCode(..))
import System.Directory (getCurrentDirectory, setCurrentDirectory,
                         doesDirectoryExist, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import System.FilePath.Glob (glob)

import qualified Data.ByteString as B
import qualified Data.Map as M

import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.Aeson as A

import qualified Text.Parsec as Parsec

import qualified Language.PureScript as P
import qualified Paths_pursuit as Paths

-- A condition that means that the generator is unable to continue.
data Error
  = DecodeLibrariesFailed T.Text
  | CommandFailed Int T.Text T.Text
  | PreludeFailed LibraryError
  deriving (Show)

-- A condition that indicates that something is wrong and should probably be
-- fixed, but is not severe enough that the generator is unable to continue.
data Warning
  = LibraryFailed Library LibraryError
  deriving (Show)

-- A condition that means that database entries for a particular library could
-- not be created.
data LibraryError
  = NoSuitableTagsFound
  | ParseFailed Parsec.ParseError
  deriving (Show)

newtype Generate a =
  G { unG :: WriterT (DL.DList Warning) (ExceptT Error IO) a }
  deriving (Functor, Applicative, Monad, MonadWriter (DL.DList Warning),
            MonadIO, MonadError Error)

runGenerate :: Generate a -> IO (Either Error (a, [Warning]))
runGenerate action =
  fmap (fmap (fmap DL.toList))
       (runExceptT (runWriterT (unG action)))

warn :: Warning -> Generate ()
warn w = tell (DL.singleton w)

getBaseDir :: IO FilePath
getBaseDir = do
  currentDir <- getCurrentDirectory
  return $ currentDir </> workingDir

generateDatabase :: FilePath -> IO (Either Error (PursuitDatabase, [Warning]))
generateDatabase = runGenerate . generateDatabase'

generateDatabase' :: FilePath -> Generate PursuitDatabase
generateDatabase' librariesFile = do
  libraries <- getLibraries librariesFile

  dbs <- getLibraryDbs libraries
  preludeDb <- buildPreludeDb

  return $ preludeDb <> mconcat dbs

getLibraryDbs :: [Library] -> Generate [PursuitDatabase]
getLibraryDbs libraries = do
  baseDir <- liftIO getBaseDir

  forM libraries $ \lib -> do
    let name = libraryName lib
    let dir = baseDir </> name
    gitClone (libraryGitUrl lib) dir
    mVers <- getMostRecentTaggedVersion dir
    case mVers of
      Nothing -> failLibrary lib NoSuitableTagsFound
      Just vers' -> do
        gitCheckoutTag vers' dir
        let vers = dropWhile (== 'v') vers'
        -- p $ "selected " ++ name ++ ": " ++ vers

        let info = libraryBowerName lib
                      >>= (\n -> Just (n, mkLibraryInfo lib vers))

        buildLibraryDb lib info dir

failLibrary :: Library -> LibraryError -> Generate PursuitDatabase
failLibrary lib reason = do
  warn (LibraryFailed lib reason)
  return mempty

workingDir :: String
workingDir = "./tmp/"

getLibraries :: FilePath -> Generate [Library]
getLibraries librariesFile = do
  json <- liftIO $ B.readFile librariesFile
  case A.eitherDecodeStrict json of
    Right libs -> return libs
    Left err -> throwError (DecodeLibrariesFailed (T.pack err))

libraryName :: Library -> String
libraryName lib =
  fromMaybe (last $ splitOn "/" $ libraryGitUrl lib) (libraryBowerName lib)

-- Clone the specified repository into the specified directory.
gitClone :: GitUrl -> FilePath -> Generate ()
gitClone url dir = do
  liftIO $ do
    exists <- doesDirectoryExist dir
    when exists $
      removeDirectoryRecursive dir

  -- p $ "cloning: " ++ url
  runCommandQuiet "git" ["clone", url, dir]

gitCheckoutTag :: String -> FilePath -> Generate ()
gitCheckoutTag tag gitDir = do
  pushd gitDir $ do
    runCommandQuiet "git" ["checkout", tag]

getMostRecentTaggedVersion :: FilePath -> Generate (Maybe String)
getMostRecentTaggedVersion gitDir = do
  pushd gitDir $ do
    (out, _) <- runCommand "git" ["tag", "--list"]
    let versions = mapMaybe parseVersion' $ lines out
    let vers = listToMaybe $ reversedSort versions
    return $ fmap snd vers

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

-- Build a subset of the full database for a single library, which may or may
-- not have a name or associated information. In all cases, the exported types
-- and values appear in the database. If both a library name and additional
-- library information are supplied, then those appear in the database too.
buildLibraryDb ::
  Library -> Maybe (String, LibraryInfo) -> FilePath -> Generate PursuitDatabase
buildLibraryDb lib extraInfo dir = do
  entriesFromDir dir >>= \case
    Left err -> failLibrary lib (ParseFailed err)
    Right entries' -> return $ databaseFromEntries extraInfo entries'

-- Build a PursuitDatabase from a list of entries, optionally also with details
-- of the library they come from.
databaseFromEntries ::
  Maybe (String, LibraryInfo) -> [PursuitEntry] -> PursuitDatabase
databaseFromEntries extraInfo entries' =
  let mLibName = fst <$> extraInfo
      mInfo    = snd <$> extraInfo
      entries  = map (\e -> e { entryLibraryName = mLibName }) entries'
      lib      = M.singleton <$> mLibName <*> mInfo
  in PursuitDatabase (fromMaybe mempty lib) entries

mkLibraryInfo :: Library -> String -> LibraryInfo
mkLibraryInfo lib version = LibraryInfo version (libraryWebUrl lib)

entriesFromDir :: FilePath -> Generate (Either Parsec.ParseError [PursuitEntry])
entriesFromDir dir = do
  files <- liftIO $ glob $ dir </> "src/**/*.purs"
  parsedFiles <- mapM parseFile files

  return (modulesToEntries . concat <$> sequence parsedFiles)

preludeWebUrl :: String
preludeWebUrl = "https://github.com/purescript/purescript/tree/master/prelude"

buildPreludeDb :: Generate PursuitDatabase
buildPreludeDb = do
  modules <- parseText "<<Prelude>>" (T.pack P.prelude) >>= \case
    Left err -> throwError (PreludeFailed (ParseFailed err))
    Right ms -> return ms

  let entries = modulesToEntries modules
  let preludeInfo = LibraryInfo (showVersion Paths.version) preludeWebUrl
  let lib = M.singleton "Prelude" preludeInfo
  return $ PursuitDatabase lib entries

modulesToEntries :: [P.Module] -> [PursuitEntry]
modulesToEntries = concatMap entriesForModule

parseFile :: FilePath -> Generate (Either Parsec.ParseError [P.Module])
parseFile input = do
  text <- liftIO $ T.readFile input
  parseText input text

parseText ::
  FilePath -> T.Text -> Generate (Either Parsec.ParseError [P.Module])
parseText input text =
  return (P.lex input (T.unpack text) >>= P.runTokenParser input P.parseModules)

entriesForModule :: P.Module -> [PursuitEntry]
entriesForModule (P.Module mn ds _) = concatMap (entriesForDeclaration mn) ds

entry :: P.ModuleName -> String -> String -> PursuitEntry
entry mn name detail = PursuitEntry name (show mn) detail Nothing

entriesForDeclaration :: P.ModuleName -> P.Declaration -> [PursuitEntry]
entriesForDeclaration mn (P.TypeDeclaration ident ty) =
  [entry mn (show ident) $ show ident ++ " :: " ++ prettyPrintType' ty]
entriesForDeclaration mn (P.ExternDeclaration _ ident _ ty) =
  [entry mn (show ident) $ show ident ++ " :: " ++ prettyPrintType' ty]
entriesForDeclaration mn (P.DataDeclaration dtype name args ctors) =
  let typeName = P.runProperName name ++ (if null args then "" else " " ++ unwords (map fst args))
      detail = show dtype ++ " " ++ typeName ++ (if null ctors then "" else " = ") ++
        intercalate " | " (map (\(ctor, tys) ->
          intercalate " " (P.runProperName ctor : map P.prettyPrintTypeAtom tys)) ctors)
  in entry mn (show name) detail : map (\(ctor, _) -> entry mn (show ctor) detail) ctors
entriesForDeclaration mn (P.ExternDataDeclaration name kind) =
  [entry mn (show name) $ "data " ++ P.runProperName name ++ " :: " ++ P.prettyPrintKind kind]
entriesForDeclaration mn (P.TypeSynonymDeclaration name args ty) =
  let typeName = P.runProperName name ++ " " ++ unwords (map fst args)
  in [entry mn (show name) $ "type " ++ typeName ++ " = " ++ prettyPrintType' ty]
entriesForDeclaration mn (P.TypeClassDeclaration name args implies ds) =
  let impliesText = case implies of
                      [] -> ""
                      is -> "(" ++ intercalate ", " (map (\(pn, tys') -> show pn ++ " " ++ unwords (map P.prettyPrintTypeAtom tys')) is) ++ ") <= "
      detail = "class " ++ impliesText ++ P.runProperName name ++ " " ++ unwords (map fst args) ++ " where"
  in entry mn (show name) detail : concatMap (entriesForDeclaration mn) ds
entriesForDeclaration mn (P.TypeInstanceDeclaration name constraints className tys _) = do
  let constraintsText = case constraints of
                          [] -> ""
                          cs -> "(" ++ intercalate ", " (map (\(pn, tys') -> show pn ++ " " ++ unwords (map P.prettyPrintTypeAtom tys')) cs) ++ ") => "
  [entry mn (show name) $ "instance " ++ show name ++ " :: " ++ constraintsText ++ show className ++ " " ++ unwords (map P.prettyPrintTypeAtom tys)]
entriesForDeclaration mn (P.PositionedDeclaration _ _ d) =
  entriesForDeclaration mn d
entriesForDeclaration _ _ = []

prettyPrintType' :: P.Type -> String
prettyPrintType' = P.prettyPrintType . P.everywhereOnTypes dePrim
  where
  dePrim ty@(P.TypeConstructor (P.Qualified _ name))
    | ty == P.tyBoolean || ty == P.tyNumber || ty == P.tyString =
      P.TypeConstructor $ P.Qualified Nothing name
  dePrim other = other
