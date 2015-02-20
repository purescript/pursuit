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
{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, LambdaCase #-}

module Pursuit.Generator (
  Error(..),
  Warning(..),
  LibraryError(..),
  generateDatabase
) where

import Data.List
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
import Control.Monad.Except

import Pursuit

import System.Exit (exitFailure, ExitCode(..))
import System.IO (stderr)
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
  | CommandFailed ExitCode T.Text T.Text
  deriving (Show, Eq)

-- A condition that indicates that something is wrong and should probably be
-- fixed, but is not severe enough that the generator is unable to continue.
data Warning
  = LibraryFailed Library LibraryError

-- A condition that means that database entries for a particular library could
-- not be created.
data LibraryError
  = NoSuitableTagsFound
  | ParseFailed Parsec.ParseError

newtype Generate a = G { unG :: WriterT [Warning] (ExceptT Error IO) a }
  deriving (Functor, Applicative, Monad, MonadWriter [Warning], MonadIO)

runGenerate :: Generate a -> IO (Either Error (a, [Warning]))
runGenerate action = do
  runExceptT (runWriterT (unG action)) >>= \case
    Right x -> return (Right x)
    Left _  -> error "lol"

p :: String -> IO ()
p = T.hPutStrLn stderr . T.pack

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
    liftIO $ gitClone (libraryGitUrl lib) dir
    mVers <- liftIO $ getMostRecentTaggedVersion dir
    case mVers of
      Nothing -> liftIO $ do
        p $ "error: no suitable tags found for " ++ name
        exitFailure
      Just vers' -> do
        let vers = dropWhile (== 'v') vers'
        liftIO $ p $ "selected " ++ name ++ ": " ++ vers
        liftIO $ gitCheckoutTag vers' dir
        let info = LibraryInfo vers (libraryWebUrl lib)
        buildLibraryDb (libraryBowerName lib) (Just info) dir

workingDir :: String
workingDir = "./tmp/"

getLibraries :: FilePath -> Generate [Library]
getLibraries librariesFile = do
  json <- liftIO $ B.readFile librariesFile
  case A.eitherDecodeStrict json of
    Right libs -> return libs
    Left err -> liftIO $ do
      T.hPutStrLn stderr (T.pack err)
      exitFailure

libraryName :: Library -> String
libraryName lib =
  fromMaybe (last $ splitOn "/" $ libraryGitUrl lib) (libraryBowerName lib)

-- Clone the specified repository into the specified directory.
gitClone :: GitUrl -> FilePath -> IO ()
gitClone url dir = do
  exists <- doesDirectoryExist dir
  when exists $
    removeDirectoryRecursive dir

  p $ "cloning: " ++ url
  runCommandQuiet "git" ["clone", url, dir]

gitCheckoutTag :: String -> FilePath -> IO ()
gitCheckoutTag tag gitDir = do
  pushd gitDir $ do
    runCommandQuiet "git" ["checkout", tag]

getMostRecentTaggedVersion :: FilePath -> IO (Maybe String)
getMostRecentTaggedVersion gitDir = do
  pushd gitDir $ do
    (out, _) <- runCommand "git" ["tag", "--list"]
    let versions = mapMaybe parseVersion' $ lines out
    let vers = listToMaybe $ reversedSort versions
    return $ fmap snd vers

pushd :: FilePath -> IO a -> IO a
pushd dir action = do
  oldDir <- getCurrentDirectory
  setCurrentDirectory dir
  result <- action
  setCurrentDirectory oldDir
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

runCommand :: FilePath -> [String] -> IO (String, String)
runCommand program args = do
  (code, out, err) <- liftIO $ readProcessWithExitCode program args ""
  case code of
    ExitSuccess -> return (out, err)
    ExitFailure _ -> do
      T.hPutStr stderr (T.pack out)
      T.hPutStr stderr (T.pack err)
      exitFailure

runCommandQuiet :: FilePath -> [String] -> IO ()
runCommandQuiet program args =
  void $ runCommand program args

-- Build a subset of the full database for a single library, which may or may
-- not have a name or associated information. In all cases, the exported types
-- and values appear in the database. If both a library name and additional
-- library information are supplied, then those appear in the database too.
buildLibraryDb ::
  Maybe String -> Maybe LibraryInfo -> FilePath -> Generate PursuitDatabase
buildLibraryDb mLibName mInfo dir = do
  entries' <- entriesFromDir dir

  let entries = map (\e -> e { entryLibraryName = mLibName }) entries'
  let lib = M.singleton <$> mLibName <*> mInfo

  return $ PursuitDatabase (fromMaybe mempty lib) entries

entriesFromDir :: FilePath -> Generate [PursuitEntry]
entriesFromDir dir = do
  files <- liftIO $ glob $ dir </> "src/**/*.purs"
  modulesToEntries . concat <$> mapM parseFile files

preludeWebUrl :: String
preludeWebUrl = "https://github.com/purescript/purescript/tree/master/prelude"

buildPreludeDb :: Generate PursuitDatabase
buildPreludeDb = do
  entries <- modulesToEntries <$> parseText "<<Prelude>>" (T.pack P.prelude)
  let preludeInfo = LibraryInfo (showVersion Paths.version) preludeWebUrl
  let lib = M.singleton "Prelude" preludeInfo
  return $ PursuitDatabase lib entries

modulesToEntries :: [P.Module] -> [PursuitEntry]
modulesToEntries = concatMap entriesForModule

parseFile :: FilePath -> Generate [P.Module]
parseFile input = do
  text <- liftIO $ T.readFile input
  parseText input text

parseText :: FilePath -> T.Text -> Generate [P.Module]
parseText input text = do
  case P.lex input (T.unpack text) >>= P.runTokenParser input P.parseModules of
    Left err -> liftIO $ do
      T.hPutStr stderr $ T.pack $ show err
      exitFailure
    Right ms -> do
      return ms

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
