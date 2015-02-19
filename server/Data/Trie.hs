module Data.Trie where

import Data.Maybe

import qualified Data.Map as M

data Trie a = Trie (Maybe a) (M.Map Char (Trie a))

instance (Show a) => Show (Trie a) where
  show (Trie a m) =
    "Trie " ++
    "{ value: " ++ show a ++
    ", children: " ++ show m ++
    " }"

toArray :: Trie a -> [(String, a)]
toArray = go ""
  where
  go s (Trie a m) = maybe [] (\x -> [(s, x)]) a ++ concatMap (\(c, t) -> go (s ++ [c]) t) (M.toList m)

empty :: Trie a
empty = Trie Nothing M.empty

insert :: String -> a -> Trie a -> Trie a
insert s a = go s
  where
  go []       (Trie _  m) = Trie (Just a) m
  go (c : cs) (Trie a1 m) = Trie a1 (M.alter (Just . go cs . fromMaybe empty) c m)

lookupAll :: String -> Trie a -> Maybe (Trie a)
lookupAll [] t@(Trie _ _) = Just t
lookupAll (c : s) (Trie _ m) = do
  t <- M.lookup c m
  lookupAll s t

lookup :: String -> Trie a -> Maybe a
lookup s t = do
  Trie result _ <- lookupAll s t
  result

