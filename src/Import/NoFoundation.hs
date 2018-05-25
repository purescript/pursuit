{-# LANGUAGE CPP #-}
module Import.NoFoundation
    ( module Import
    , hush
    ) where

import ClassyPrelude.Yesod   as Import
import Settings              as Import
import Model                 as Import
import Settings              as Import
import Yesod.Core.Types      as Import (loggerSet)
import Yesod.Default.Config2 as Import
import Yesod.Core.Types      as Import (loggerSet)
import Control.Category      as Import ((>>>), (<<<))

hush :: Either a b -> Maybe b
hush = either (const Nothing) Just
