module SearchSpec where

import Import.NoFoundation
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck as QC

import SearchUtils (interleave)

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

spec :: Spec
spec = do
  prop "interleave is associative" $
    forAll ((,,) <$> genAssocList <*> genAssocList <*> genAssocList) $
      \(xs, ys, zs) ->
        (xs `interleave` ys) `interleave` zs ===
        xs `interleave` (ys `interleave` zs)

  prop "[] is identity for interleave" $
    forAll genAssocList $ \xs ->
      QC.conjoin
        [ xs `interleave` [] === xs
        , [] `interleave` xs === xs
        ]

  prop "interleave preserves ordering" $
    forAll ((,) <$> genSortedAssocList <*> genSortedAssocList) $
      \(xs, ys) ->
        let
          zs = xs `interleave` ys
        in
          zs === sortWith snd zs
