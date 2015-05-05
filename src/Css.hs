
module Css where

import Import.NoFoundation
import Text.Lucius (ToCss(..))

data Color = Color
  { colorR :: Int
  , colorG :: Int
  , colorB :: Int
  , colorA :: Maybe Int
  }
  deriving (Show, Eq, Ord)

colorRGB :: Int -> Int -> Int -> Color
colorRGB r g b = Color r g b Nothing

render :: Color -> Text
render Color{..} =
  let rgb = intercalate ", " (map tshow [colorR, colorG, colorB])
  in case colorA of
    Just a -> "rgba(" <> rgb <> ", " <> tshow a <> ")"
    Nothing -> "rgb(" <> rgb <> ")"

instance ToCss Color where
  toCss = toCss . render

bannerBackground :: Color
bannerBackground = colorRGB 29 34 45

