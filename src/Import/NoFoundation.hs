module Import.NoFoundation
    ( module Import.NoFoundation
    , module Import
    ) where

import ClassyPrelude.Yesod   as Import hiding (intercalate)
import Settings              as Import
import Yesod.Core.Types      as Import (loggerSet)
import Yesod.Default.Config2 as Import
import Control.Category      as Import ((>>>), (<<<))

type One = Succ Zero

-- This is a more useful way of defining intercalate, imo.
intercalate :: (Monoid (Element seq), SemiSequence seq) =>
  Element seq -> seq -> Element seq
intercalate x xs = concat (intersperse x xs)
