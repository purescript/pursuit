module Data.Trie where

import Data.Maybe
import Data.Array
import Data.Tuple

import qualified Data.Map as M
import qualified Data.String as S

data Trie a = Trie (Maybe a) (M.Map String (Trie a))

instance showTrie :: (Show a) => Show (Trie a) where
  show (Trie a m) =
    "Trie " ++
    "{ value: " ++ show a ++
    ", children: " ++ show m ++
    " }"

toArray :: forall a. Trie a -> [Tuple String a]
toArray = go ""
  where
  go s (Trie a m) = maybe [] (\a' -> [Tuple s a']) a <> concatMap (\(Tuple c t) -> go (s <> c) t) (M.toList m)

empty :: forall a. Trie a
empty = Trie Nothing M.empty

insert :: forall a. String -> a -> Trie a -> Trie a
insert s a = go 0
  where
  go i (Trie _  m) | i >= S.length s = Trie (Just a) m
  go i (Trie a1 m) = Trie a1 (M.alter (Just <<< go (i + 1) <<< fromMaybe empty) (S.charAt i s) m)

lookupAll :: forall a. String -> Trie a -> Maybe (Trie a)
lookupAll s = go 0
  where
  go i t@(Trie _ _) | i >= S.length s = Just t
  go i (Trie _ m) = do
    t <- M.lookup (S.charAt i s) m
    go (i + 1) t

lookup :: forall a. String -> Trie a -> Maybe a
lookup s t = do
  Trie result _ <- lookupAll s t
  result

