-----------------------------------------------------------------------------
--
-- Module      :  Main
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

module Main where

import Data.List
import Data.Ord
import Data.List.Split (splitOn)
import Data.Maybe
import Data.Monoid
import Data.Version (showVersion, parseVersion, Version)
import Text.ParserCombinators.ReadP (readP_to_S)

import Control.Applicative
import Control.Monad

import System.Console.CmdTheLine
import System.Exit (exitSuccess, exitFailure, ExitCode(..))
import System.IO (stderr)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory,
                         setCurrentDirectory, doesDirectoryExist,
                         removeDirectoryRecursive)
import System.FilePath (takeDirectory, (</>))
import System.Process (readProcessWithExitCode)
import System.FilePath.Glob (glob)

import qualified Data.ByteString as B
import qualified Data.Map as M

import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as TL

import Data.Aeson ((.=), (.:), (.:?))
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode as A

import qualified Language.PureScript as P
import qualified Paths_pursuit_gen as Paths

type GitUrl = String

-- The structure of one entry in the input libraries.json file.
data Library = Library { libraryGitUrl :: GitUrl
                       , libraryBowerName :: Maybe String
                       }

instance A.FromJSON Library where
  parseJSON (A.Object o) =
    Library <$> o .: "gitUrl"
            <*> o .:? "bowerName"
  parseJSON val = fail $ "couldn't parse " ++ show val ++ " as Library"

-- The data associated with a named library in the output.
data LibraryInfo = LibraryInfo { infoVersion :: String }

instance A.FromJSON LibraryInfo where
  parseJSON (A.Object o) =
    LibraryInfo <$> o .: "version"
  parseJSON val = fail $ "couldn't parse " ++ show val ++ " as LibraryInfo"

instance A.ToJSON LibraryInfo where
  toJSON (LibraryInfo vers) =
    A.object [ "version" .= vers
             ]

data PursuitEntry =
  PursuitEntry { entryName           :: String
               , entryModule         :: String
               , entryDetail         :: String
               , entryLibraryName    :: Maybe String
               }
               deriving (Show, Eq)

instance A.FromJSON PursuitEntry where
  parseJSON (A.Object o) =
    PursuitEntry <$> o .: "name"
                 <*> o .: "module"
                 <*> o .: "detail"
                 <*> o .:? "library"
  parseJSON val = fail $ "couldn't parse " ++ show val ++ " as PursuitEntry"

instance A.ToJSON PursuitEntry where
  toJSON (PursuitEntry name mdl detail mLibName) = A.object pairs
    where
    pairs = [ "name"   .= name
            , "module" .= mdl
            , "detail" .= detail
            ] ++ maybe [] (\x -> [ "library" .= x ]) mLibName

data PursuitDatabase =
  PursuitDatabase (M.Map String LibraryInfo) [PursuitEntry]

instance A.FromJSON PursuitDatabase where
  parseJSON (A.Object o) =
    PursuitDatabase <$> o .: "libraries" <*> o .: "entries"
  parseJSON val = fail $ "couldn't parse " ++ show val ++ " as PursuitDatabase"

instance A.ToJSON PursuitDatabase where
  toJSON (PursuitDatabase libs entries) = A.object pairs
    where
    pairs = [ "libraries" .= libs
            , "entries" .= entries
            ]

instance Monoid PursuitDatabase where
  mempty = PursuitDatabase mempty mempty
  mappend (PursuitDatabase aLibs aEntries) (PursuitDatabase bLibs bEntries) =
    PursuitDatabase (aLibs <> bLibs) (aEntries <> bEntries)


p :: String -> IO ()
p = T.hPutStrLn stderr . T.pack

pursuitGenAll :: Maybe FilePath -> IO ()
pursuitGenAll output = do
  entries <- generateAllData
  let json = databaseToJson entries
  case output of
    Just path -> mkdirp path >> TL.writeFile path json
    Nothing -> TL.putStrLn json
  exitSuccess

getBaseDir :: IO FilePath
getBaseDir = do
  currentDir <- getCurrentDirectory
  return $ currentDir </> workingDir

generateAllData :: IO PursuitDatabase
generateAllData = do
  baseDir <- getBaseDir
  libraries <- getLibraries

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
        let info = LibraryInfo vers
        buildLibraryDb (libraryBowerName lib) (Just info) dir

  preludeDb <- buildPreludeDb

  return $ preludeDb <> mconcat dbs

workingDir :: String
workingDir = "./tmp/"

getLibraries :: IO [Library]
getLibraries = do
  json <- B.getContents
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

buildPreludeDb :: IO PursuitDatabase
buildPreludeDb = do
  entries <- modulesToEntries <$> parseText "<<Prelude>>" (T.pack P.prelude)
  let preludeInfo = LibraryInfo (showVersion Paths.version)
  let lib = M.singleton "PureScript" preludeInfo
  return $ PursuitDatabase lib entries

modulesToEntries :: [P.Module] -> [PursuitEntry]
modulesToEntries = concatMap entriesForModule

databaseToJson :: PursuitDatabase -> TL.Text
databaseToJson = TL.toLazyText . A.encodeToTextBuilder . A.toJSON

parseFile :: FilePath -> IO [P.Module]
parseFile input = do
  text <- T.readFile input
  parseText input text

parseText :: FilePath -> T.Text -> IO [P.Module]
parseText input text = do
  case P.runIndentParser input P.parseModules (T.unpack text) of
    Left err -> do
      T.hPutStr stderr $ T.pack $ show err
      exitFailure
    Right ms -> do
      return ms

mkdirp :: FilePath -> IO ()
mkdirp = createDirectoryIfMissing True . takeDirectory

entriesToJson :: [PursuitEntry] -> TL.Text
entriesToJson = TL.toLazyText . A.encodeToTextBuilder . A.toJSON

entriesForModule :: P.Module -> [PursuitEntry]
entriesForModule m@(P.Module mn _ exps) = concatMap (entriesForDeclaration exportedCtorsFor mn) (P.exportedDeclarations m)
  where
  exportedCtorsFor = P.exportedDctors m

entry :: P.ModuleName -> String -> String -> PursuitEntry
entry mn name detail = PursuitEntry name (show mn) detail Nothing

entriesForDeclaration :: (P.ProperName -> [P.ProperName]) -> P.ModuleName -> P.Declaration -> [PursuitEntry]
entriesForDeclaration _ mn (P.TypeDeclaration ident ty) =
  [entry mn (show ident) $ show ident ++ " :: " ++ prettyPrintType' ty]
entriesForDeclaration _ mn (P.ExternDeclaration _ ident _ ty) =
  [entry mn (show ident) $ show ident ++ " :: " ++ prettyPrintType' ty]
entriesForDeclaration exportedCtorsFor mn (P.DataDeclaration dtype name args _) =
  let ctors = exportedCtorsFor name
      typeName = P.runProperName name ++ (if null args then "" else " " ++ unwords (map fst args))
      detail = show dtype ++ " " ++ typeName ++ (if null ctors then "" else " = ") ++
        intercalate " | " (map (\(ctor, tys) ->
          intercalate " " (P.runProperName ctor : map P.prettyPrintTypeAtom tys)) ctors)
  in entry mn (show name) detail : map (\(ctor, _) -> entry mn (show ctor) detail) ctors
entriesForDeclaration _ mn (P.ExternDataDeclaration name kind) =
  [entry mn (show name) $ "data " ++ P.runProperName name ++ " :: " ++ P.prettyPrintKind kind]
entriesForDeclaration mn (P.TypeSynonymDeclaration name args ty) =
  let typeName = P.runProperName name ++ " " ++ unwords (map fst args)
  in [entry mn (show name) $ "type " ++ typeName ++ " = " ++ prettyPrintType' ty]
entriesForDeclaration _ mn (P.TypeClassDeclaration name args implies ds) =
  let impliesText = case implies of
                      [] -> ""
                      is -> "(" ++ intercalate ", " (map (\(pn, tys') -> show pn ++ " " ++ unwords (map P.prettyPrintTypeAtom tys')) is) ++ ") <= "
      detail = "class " ++ impliesText ++ P.runProperName name ++ " " ++ unwords (map fst args) ++ " where"
  in entry mn (show name) detail : concatMap (entriesForDeclaration mn) ds
entriesForDeclaration _ mn (P.TypeInstanceDeclaration name constraints className tys _) = do
  let constraintsText = case constraints of
                          [] -> ""
                          cs -> "(" ++ intercalate ", " (map (\(pn, tys') -> show pn ++ " " ++ unwords (map P.prettyPrintTypeAtom tys')) cs) ++ ") => "
  [entry mn (show name) $ "instance " ++ show name ++ " :: " ++ constraintsText ++ show className ++ " " ++ unwords (map P.prettyPrintTypeAtom tys)]
entriesForDeclaration _ mn (P.PositionedDeclaration _ d) =
  entriesForDeclaration mn d
entriesForDeclaration _ _ _ = []

prettyPrintType' :: P.Type -> String
prettyPrintType' = P.prettyPrintType . P.everywhereOnTypes dePrim
  where
  dePrim ty@(P.TypeConstructor (P.Qualified _ name))
    | ty == P.tyBoolean || ty == P.tyNumber || ty == P.tyString =
      P.TypeConstructor $ P.Qualified Nothing name
  dePrim other = other

outputFile :: Term (Maybe FilePath)
outputFile = value $ opt Nothing $ (optInfo [ "o", "output" ]) { optDoc = "The output .json file" }

term :: Term (IO ())
term = pursuitGenAll <$> outputFile

termInfo :: TermInfo
termInfo = defTI
  { termName = "pursuit-gen"
  , version  = showVersion Paths.version
  , termDoc  = "Generate data for use with the pursuit search engine"
  }

main :: IO ()
main = run (term, termInfo)
