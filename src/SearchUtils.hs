-- | Useful stuff which doesn't need to be in Handler.Search
module SearchUtils where

import Import.NoFoundation

import qualified Language.PureScript as P

-- | Interleave two lists. If the arguments are in ascending order (according
-- to their second elements) then the result is also in ascending order.
interleave :: Ord score => [(a,score)] -> [(a,score)] -> [(a,score)]
interleave [] ys = ys
interleave xs [] = xs
interleave (x@(_, scoreX):xs) (y@(_, scoreY):ys) =
  if scoreX > scoreY
    then
      y : interleave (x:xs) ys
    else
      x : interleave xs (y:ys)

-- This is an approximation to type subsumption / unification.
-- This function returns Just a score if there is a possible match,
-- or Nothing otherwise. Lower scores are better.
compareTypes :: P.Type -> P.Type -> Maybe Int
compareTypes (P.TypeVar _) (P.TypeVar _) = Just 0
compareTypes t (P.TypeVar _) = Just (1 + typeComplexity t)
compareTypes (P.TypeLevelString s1) (P.TypeLevelString s2) | s1 == s2 = Just 0
compareTypes (P.TypeWildcard _) t = Just (typeComplexity t)
compareTypes (P.TypeConstructor q1) (P.TypeConstructor q2) | compareQual q1 q2 = Just 0
-- There is a special case for functions, since if the user _asked_ for a function,
-- they probably don't want to see something more general of type 'f a' or 'f a b'.
compareTypes (P.TypeApp a b) (P.TypeApp c d)
  | not (isFunction a && not (isFunction c)) = (+) <$> compareTypes a c <*> compareTypes b d
compareTypes (P.ForAll _ t1 _) t2 = compareTypes t1 t2
compareTypes t1 (P.ForAll _ t2 _) = compareTypes t1 t2
compareTypes (P.ConstrainedType _ t1) t2 = compareTypes t1 t2
compareTypes t1 (P.ConstrainedType _ t2) = compareTypes t1 t2
compareTypes P.REmpty P.REmpty = Just 0
compareTypes t1@P.RCons{} t2 = compareRows t1 t2
compareTypes t1 t2@P.RCons{} = compareRows t1 t2
compareTypes (P.KindedType t1 _) t2 = compareTypes t1 t2
compareTypes t1 (P.KindedType t2 _) = compareTypes t1 t2
-- Really, we should desugar any type operators here.
-- Since type operators are not supported in search right now, this is fine,
-- since we only care about functions, which are already in the correct
-- order as they come out of the parser.
compareTypes (P.ParensInType t1) t2 = compareTypes t1 t2
compareTypes t1 (P.ParensInType t2) = compareTypes t1 t2
compareTypes _ _ = Nothing

isFunction :: P.Type -> Bool
isFunction (P.TypeConstructor (P.Qualified _ (P.ProperName "Function"))) = True
isFunction _ = False

compareRows :: P.Type -> P.Type -> Maybe Int
compareRows r1 r2 = sum <$>
  sequence [ compareTypes t1 t2
           | (name, t1) <- fst (P.rowToList r1)
           , (name', t2) <- fst (P.rowToList r2)
           , name == name'
           ]

typeComplexity :: P.Type -> Int
typeComplexity (P.TypeApp a b) = 1 + typeComplexity a + typeComplexity b
typeComplexity (P.ForAll _ t _) = 1 + typeComplexity t
typeComplexity (P.ConstrainedType _ t) = typeComplexity t + 1
typeComplexity P.REmpty = 0
typeComplexity (P.RCons _ t r) = 1 + typeComplexity t + typeComplexity r
typeComplexity (P.KindedType t _) = typeComplexity t
typeComplexity (P.ParensInType t) = typeComplexity t
typeComplexity _ = 0

compareQual :: Eq a => P.Qualified a -> P.Qualified a -> Bool
compareQual (P.Qualified (Just mn1) a1) (P.Qualified (Just mn2) a2) = mn1 == mn2 && a1 == a2
compareQual (P.Qualified _ a1) (P.Qualified _ a2) = a1 == a2
