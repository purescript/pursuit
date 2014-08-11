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

module Main where

import Data.List
import Data.String (fromString)
import Data.Version (showVersion)

import Control.Applicative

import System.Console.CmdTheLine
import System.Exit (exitSuccess, exitFailure)
import System.IO (stderr)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as TL

import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode as A

import qualified Language.PureScript as P
import qualified Paths_pursuit_gen as Paths

pursuitGen :: [FilePath] -> Maybe FilePath -> IO ()
pursuitGen input output = do
  ms <- mapM parseFile (nub input)
  let json = modulesToJson (concat ms)
  case output of
    Just path -> mkdirp path >> TL.writeFile path json
    Nothing -> TL.putStrLn json
  exitSuccess

parseFile :: FilePath -> IO [P.Module]
parseFile input = do
  text <- T.readFile input
  case P.runIndentParser input P.parseModules (T.unpack text) of
    Left err -> do
      T.hPutStr stderr $ T.pack $ show err
      exitFailure
    Right ms -> do
      return ms

mkdirp :: FilePath -> IO ()
mkdirp = createDirectoryIfMissing True . takeDirectory

array :: [A.Value] -> A.Value
array = A.toJSON

modulesToJson :: [P.Module] -> TL.Text
modulesToJson = TL.toLazyText . A.encodeToTextBuilder . array . concatMap jsonForModule

jsonForModule :: P.Module -> [A.Value]
jsonForModule (P.Module mn ds _) = concatMap (jsonForDeclaration mn) ds

entry :: P.ModuleName -> String -> String -> A.Value
entry mn name detail =
  A.object [ fromString "name"   .= name
           , fromString "module" .= show mn
           , fromString "detail" .= detail
           ]

jsonForDeclaration :: P.ModuleName -> P.Declaration -> [A.Value]
jsonForDeclaration mn (P.TypeDeclaration ident ty) =
  [entry mn (show ident) $ show ident ++ " :: " ++ prettyPrintType' ty]
jsonForDeclaration mn (P.ExternDeclaration _ ident _ ty) =
  [entry mn (show ident) $ show ident ++ " :: " ++ prettyPrintType' ty]
jsonForDeclaration mn (P.DataDeclaration dtype name args ctors) =
  let typeName = P.runProperName name ++ (if null args then "" else " " ++ unwords args)
      detail = show dtype ++ " " ++ typeName ++ (if null ctors then "" else " = ") ++
        intercalate " | " (map (\(ctor, tys) ->
          P.runProperName ctor ++ " :: " ++ concatMap (\ty -> prettyPrintType' ty ++ " -> ") tys ++ typeName) ctors)
  in entry mn (show name) detail : map (\(ctor, _) -> entry mn (show ctor) detail) ctors
jsonForDeclaration mn (P.ExternDataDeclaration name kind) =
  [entry mn (show name) $ "data " ++ P.runProperName name ++ " :: " ++ P.prettyPrintKind kind]
jsonForDeclaration mn (P.TypeSynonymDeclaration name args ty) =
  let typeName = P.runProperName name ++ " " ++ unwords args
  in [entry mn (show name) $ "type " ++ typeName ++ " = " ++ prettyPrintType' ty]
jsonForDeclaration mn (P.TypeClassDeclaration name args implies ds) =
  let impliesText = case implies of
                      [] -> ""
                      is -> "(" ++ intercalate ", " (map (\(pn, tys') -> show pn ++ " " ++ unwords (map P.prettyPrintTypeAtom tys')) is) ++ ") <= "
      detail = "class " ++ impliesText ++ P.runProperName name ++ " " ++ unwords args ++ " where"
  in entry mn (show name) detail : concatMap (jsonForDeclaration mn) ds
jsonForDeclaration mn (P.TypeInstanceDeclaration name constraints className tys _) = do
  let constraintsText = case constraints of
                          [] -> ""
                          cs -> "(" ++ intercalate ", " (map (\(pn, tys') -> show pn ++ " " ++ unwords (map P.prettyPrintTypeAtom tys')) cs) ++ ") => "
  [entry mn (show name) $ "instance " ++ show name ++ " :: " ++ constraintsText ++ show className ++ " " ++ unwords (map P.prettyPrintTypeAtom tys)]
jsonForDeclaration mn (P.PositionedDeclaration _ d) =
  jsonForDeclaration mn d
jsonForDeclaration _ _ = []

prettyPrintType' :: P.Type -> String
prettyPrintType' = P.prettyPrintType . P.everywhereOnTypes dePrim
  where
  dePrim ty@(P.TypeConstructor (P.Qualified _ name))
    | ty == P.tyBoolean || ty == P.tyNumber || ty == P.tyString =
      P.TypeConstructor $ P.Qualified Nothing name
  dePrim other = other

inputFiles :: Term [FilePath]
inputFiles = value $ posAny [] $ posInfo { posName = "file(s)", posDoc = "The input .purs file(s)" }

outputFile :: Term (Maybe FilePath)
outputFile = value $ opt Nothing $ (optInfo [ "o", "output" ]) { optDoc = "The output .json file" }

term :: Term (IO ())
term = pursuitGen <$> inputFiles <*> outputFile

termInfo :: TermInfo
termInfo = defTI
  { termName = "pursuit-gen"
  , version  = showVersion Paths.version
  , termDoc  = "Generate data for use with the pursuit search engine"
  }

main :: IO ()
main = run (term, termInfo)
