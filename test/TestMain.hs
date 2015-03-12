{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Control.Monad

import System.Exit (exitFailure)

import Test.Hspec

import Pursuit

testLibrariesFile :: String
testLibrariesFile = "./test/libraries-minimal.json"

getDatabase :: IO PursuitDatabase
getDatabase = do
  (warns, _, eitherDb) <- generateDatabase testLibrariesFile

  if (null warns)
    then putStrLn "Generated database. No warnings."
    else mapM_ (putStrLn . show) warns

  case eitherDb of
    Right db -> return db
    Left err -> putStrLn (show err) >> exitFailure

main :: IO ()
main = do
  db <- getDatabase
  let query q = runQuery (queryDeclsJ q) db
  
  hspec $ do
    describe "ADTs" $ do
      describe "with non-exported data constructors" $ do
        specify "are not included in results" $ do
          query "ThisShouldNotBeExported" `shouldBe` []
          query "NeitherShouldThis" `shouldBe` []

      describe "with exported data constructors" $ do
        specify "are included in results" $ do
          query "ThisShouldBeExported" `shouldSatisfy` not . null
          query "SoShouldThis" `shouldSatisfy` not . null

    describe "Values" $ do
      describe "which are exported" $ do
        specify "should be included in results" $ do
          query "exportedFn" `shouldSatisfy` not . null

      describe "which are not exported" $ do
        specify "should not be included in results" $ do
          query "nonExportedFn" `shouldBe` []

      describe "without type signatures" $ do
        specify "should still be included" $ do
          query "withoutTypeSig" `shouldSatisfy` not . null

      describe "with symbolic names" $ do
        specify "should not require parenthesis" $ do
          let withParens = query "(<++>)"
          let noParens   = query "<++>"

          withParens `shouldSatisfy` not . null
          withParens `shouldBe` noParens

      specify "should not be case sensitive" $ do
        let q = "ThisShouldBeExported"
        query q `shouldBe` query (T.toLower q)

    describe "Typeclass members" $ do
      specify "should include the correct constraints in their types" $ do
        let result = map (\(d, _, _) -> d) (query "thingy")
        let thingy' = filter ((== DeclName "thingy") . declName) result

        thingy <- exactlyOne thingy'
        runDeclDetail (declDetail thingy) `shouldHaveSubstring` "(Thingy a) =>"
    
    describe "Primitive types" $ do
      describe "should appear in the results" $ do
        forM_ ["Function", "String", "Number", "Array", "Object", "Boolean"] $ \prim -> do
          specify (T.unpack prim) $
            query prim `shouldSatisfy` not . null

  where
  exactlyOne :: [a] -> IO a
  exactlyOne [x] = return x
  exactlyOne _ = expectationFailure "expected a list with exactly one element"
                  >> return undefined

  shouldHaveSubstring str substr =
    str `shouldSatisfy` (not . null) . TL.breakOnAll substr
