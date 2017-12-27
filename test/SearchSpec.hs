module SearchSpec where

import Import.NoFoundation
import Data.Maybe (fromJust)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck as QC

import Handler.Search (interleave)
import SearchIndex (compareTypes, parseType)

data Whatev = A | B | C | D | E
  deriving (Show, Eq, Enum, Bounded)

genAssocList :: QC.Gen [(Whatev, Int)]
genAssocList =
  QC.listOf $
    (,) <$> QC.elements [minBound..maxBound]
        <*> QC.choose (0,20)

genSortedAssocList :: QC.Gen [(Whatev, Int)]
genSortedAssocList =
  fmap (sortWith snd) genAssocList

-- A version of 'expectationFailure' with its type relaxed from IO () to IO a.
failed :: String -> IO a
failed msg =
  expectationFailure msg
  >> error "should not be reached; hack to relax the type of expectationFailure"

shouldBeLessThan :: (Show a, Ord a) => a -> a -> Expectation
shouldBeLessThan x y =
  if x < y
    then pure ()
    else expectationFailure
           (show x <> " should have been less than " <> show y)

shouldBeGreaterThan :: (Show a, Ord a) => a -> a -> Expectation
shouldBeGreaterThan x y =
  if x > y
    then pure ()
    else expectationFailure
           (show x <> " should have been greater than " <> show y)

spec :: Spec
spec = do
  describe "interleave" $ do
    prop "is associative" $
      forAll ((,,) <$> genAssocList <*> genAssocList <*> genAssocList) $
        \(xs, ys, zs) ->
          (xs `interleave` ys) `interleave` zs ===
          xs `interleave` (ys `interleave` zs)

    prop "has identity []" $
      forAll genAssocList $ \xs ->
        QC.conjoin
          [ xs `interleave` [] === xs
          , [] `interleave` xs === xs
          ]

    prop "preserves ordering" $
      forAll ((,) <$> genSortedAssocList <*> genSortedAssocList) $
        \(xs, ys) ->
          let
            zs = xs `interleave` ys
          in
            zs === sortWith snd zs

  describe "compareTypes" $ do
    let
      p = fromJust . parseType

      -- Assert that two types should return a possible match with
      -- compareTypes, and return the result.
      shouldMatch t1 t2 =
        maybe
          (failed "should have returned a possible match")
          pure
          (compareTypes t1 t2)

    it "respects alpha-equivalence" $ do
      let
        query = p "a -> b -> c -> b -> a"
        cand0 = p "d -> e -> f -> e -> d"

      shouldMatch query query >>= (`shouldBe` 0)
      shouldMatch query cand0 >>= (`shouldBe` 0)

    describe "with just type variables" $ do
      let
        query =           p "a -> b -> c -> b -> a"
        moreGeneral =     p "d -> e -> f -> e -> h"
        evenMoreGeneral = p "d -> e -> f -> g -> h"
        lessGeneral =     p "a -> b -> b -> b -> a"
        evenLessGeneral = p "a -> a -> a -> a -> a"

      it "prioritises closer matches (more general)" $ do
        x <- shouldMatch query moreGeneral
        y <- shouldMatch query evenMoreGeneral
        x `shouldBeLessThan` y

      it "prioritises closer matches (less general)" $ do
        x <- shouldMatch query lessGeneral
        y <- shouldMatch query evenLessGeneral
        x `shouldBeLessThan` y

      it "prefers more general results to less general ones" $ do
        x <- shouldMatch query evenMoreGeneral
        y <- shouldMatch query lessGeneral
        x `shouldBeLessThan` y

    it "with both type variables and concrete types" $ do
      let
        query = p "a -> b -> Int"
        cand0 = p "a -> a -> Int"
        cand1 = p "a -> a -> b"

      shouldMatch query query >>= (`shouldBe` 0)
      shouldMatch query cand0 >>= (`shouldBeGreaterThan` 0)

      x <- shouldMatch query cand0
      y <- shouldMatch query cand1
      x `shouldBeLessThan` y
