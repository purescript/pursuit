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
{-# LANGUAGE OverloadedStrings #-}

module Pursuit.Generator (
  GenerateError,
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

import qualified Language.PureScript as P
import qualified Paths_pursuit as Paths

-- TODO
data GenerateError = GenerateError
  deriving (Show, Eq)

p :: String -> IO ()
p = T.hPutStrLn stderr . T.pack

getBaseDir :: IO FilePath
getBaseDir = do
  currentDir <- getCurrentDirectory
  return $ currentDir </> workingDir

generateDatabase :: FilePath -> IO (Either GenerateError PursuitDatabase)
generateDatabase librariesFile = do
  baseDir <- getBaseDir
  libraries <- getLibraries librariesFile

  dbs <- forM libraries $ \lib -> do
    let name = libraryName lib
    let dir = baseDir </> name
    gitClone (libraryGitUrl lib) dir
    mVers <- getMostRecentTaggedVersion dir
    case mVers of
      Nothing -> do
        p $ "error: no suitable tags found for " ++ name
        exitFailure
      Just vers' -> do
        let vers = dropWhile (== 'v') vers'
        p $ "selected " ++ name ++ ": " ++ vers
        gitCheckoutTag vers' dir
        let info = LibraryInfo vers (libraryWebUrl lib)
        buildLibraryDb (libraryBowerName lib) (Just info) dir

  preludeDb <- buildPreludeDb

  return $ Right $ preludeDb <> mconcat dbs

workingDir :: String
workingDir = "./tmp/"

getLibraries :: FilePath -> IO [Library]
getLibraries librariesFile = do
  json <- B.readFile librariesFile
  case A.eitherDecodeStrict json of
    Right libs -> return libs
    Left err -> do
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
  (code, out, err) <- readProcessWithExitCode program args ""
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
  Maybe String -> Maybe LibraryInfo -> FilePath -> IO PursuitDatabase
buildLibraryDb mLibName mInfo dir = do
  entries' <- entriesFromDir dir

  let entries = map (\e -> e { entryLibraryName = mLibName }) entries'
  let lib = M.singleton <$> mLibName <*> mInfo

  return $ PursuitDatabase (fromMaybe mempty lib) entries

entriesFromDir :: FilePath -> IO [PursuitEntry]
entriesFromDir dir = do
  files <- glob $ dir </> "src/**/*.purs"
  ms <- mapM parseFile files
  return $ modulesToEntries (concat ms)

preludeWebUrl :: String
preludeWebUrl = "https://github.com/purescript/purescript/tree/master/prelude"

buildPreludeDb :: IO PursuitDatabase
buildPreludeDb = do
  entries <- modulesToEntries <$> parseText "<<Prelude>>" (T.pack P.prelude)
  let preludeInfo = LibraryInfo (showVersion Paths.version) preludeWebUrl
  let lib = M.singleton "Prelude" preludeInfo
  return $ PursuitDatabase lib entries

modulesToEntries :: [P.Module] -> [PursuitEntry]
modulesToEntries = concatMap entriesForModule

parseFile :: FilePath -> IO [P.Module]
parseFile input = do
  text <- T.readFile input
  parseText input text

parseText :: FilePath -> T.Text -> IO [P.Module]
parseText input text = do
  case P.lex input (T.unpack text) >>= P.runTokenParser input P.parseModules of
    Left err -> do
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
