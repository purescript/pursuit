
module Css where

import Import.NoFoundation
import qualified Text.Lucius as Lucius
import qualified Data.Colour.RGBSpace as C
import qualified Data.Colour.RGBSpace.HSV as C

newtype HSV = HSV (Double, Double, Double)
  deriving (Show)

type RGB = C.RGB Double

fromRGB :: Word8 -> Word8 -> Word8 -> HSV
fromRGB r g b = HSV (C.hsvView (C.RGB (f r) (f g) (f b)))
  where
  f = (/255) . fromIntegral

hsvToRGB :: HSV -> RGB
hsvToRGB (HSV (h,s,v)) = C.hsv h s v

rgbToLucius :: RGB -> Lucius.Color
rgbToLucius (C.RGB r g b) = Lucius.Color (f r) (f g) (f b)
  where
  f = floor . (*255)

instance Lucius.ToCss HSV where
  toCss = Lucius.toCss . rgbToLucius . hsvToRGB

clamp :: (Double, Double) -> Double -> Double
clamp (lo, hi) x
  | x < lo    = lo
  | x > hi    = hi
  | otherwise = x

withV :: (Double -> Double) -> HSV -> HSV
withV f (HSV (h, s, v)) = HSV (h, s, clamp (0, 1) (f v))

lighten :: Double -> HSV -> HSV
lighten amt = withV (\v -> v + amt)

lighten10 :: HSV -> HSV
lighten10 = lighten 0.1

darken :: Double -> HSV -> HSV
darken amt = withV (\v -> v - amt)

darken5 :: HSV -> HSV
darken5 = darken 0.05

darken10 :: HSV -> HSV
darken10 = darken 0.1

--------------------------------------
-- Variables for the CSS for Pursuit

bannerBackground :: HSV
bannerBackground = fromRGB 29 34 45

packageBannerBackground :: HSV
packageBannerBackground = lighten 0.3 bannerBackground

darkForeground :: HSV
darkForeground = fromRGB 240 240 240
