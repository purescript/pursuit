
module TimeUtils
  ( module Data.Time.Clock
  , module TimeUtils
  ) where

import Prelude
import Data.Time.Clock

oneDay :: NominalDiffTime
oneDay = oneHour * 24

oneHour :: NominalDiffTime
oneHour = 60 * 60

subUTCTime :: UTCTime -> NominalDiffTime -> UTCTime
subUTCTime x y = addUTCTime (negate y) x

getElapsedTimeSince :: UTCTime -> IO NominalDiffTime
getElapsedTimeSince time = fmap (flip diffUTCTime time) getCurrentTime
