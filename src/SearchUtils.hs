-- | Useful stuff which doesn't need to be in Handler.Search
module SearchUtils where

import Import.NoFoundation

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
