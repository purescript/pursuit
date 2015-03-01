module Main where

import System.Exit (exitFailure)

import Test.Hspec

import Pursuit

testLibrariesFile :: String
testLibrariesFile = "./test/libraries-minimal.json"

getDatabase :: IO PursuitDatabase
getDatabase = do
  (warns, logs, eitherDb) <- generateDatabase testLibrariesFile

  if (null warns)
	then putStrLn "Generated database. No warnings."
	else mapM_ (putStrLn . show) warns

  case eitherDb of
	Right db -> return db
	Left err -> putStrLn (show err) >> exitFailure

main :: IO ()
main = do
  db <- getDatabase
  hspec $ do
	return ()
