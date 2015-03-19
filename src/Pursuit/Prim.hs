{-# LANGUAGE TemplateHaskell #-}

module Pursuit.Prim where

import Data.FileEmbed (embedFile)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

primModule :: Text
primModule = decodeUtf8 $(embedFile "prim/Prim.purs")
