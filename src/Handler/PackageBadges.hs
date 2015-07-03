
module Handler.PackageBadges where

import Import
import Data.Version
import Text.Blaze (toValue)
import Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.Text (renderSvg)

import Handler.Packages (getLatestVersion)
import Handler.Caching (cacheSvg)

newtype ContentSvg = ContentSvg { runContentSvg :: S.Svg }

instance ToContent ContentSvg where
  toContent = toContent . renderSvg . runContentSvg

instance ToTypedContent ContentSvg where
  toTypedContent = TypedContent typeSvg . toContent

getPackageBadgeR :: PathPackageName -> Handler ContentSvg
getPackageBadgeR (PathPackageName pkgName) =
  map ContentSvg $ cacheSvg $ do
    msvg <- (map . map) renderBadge (getLatestVersion pkgName)
    case msvg of
      Just svg -> return svg
      Nothing  -> notFound

renderBadge :: Version -> S.Svg
renderBadge version =
  S.docTypeSvg ! A.version "1.1" ! A.width (i totalWidth) ! A.height (i totalHeight) $ do
    S.defs $ do
      S.clippath ! A.id_ "clipPath" $
        S.rect ! A.x (i 0) ! A.y (i 0) ! A.width (i totalWidth) ! A.height (i totalHeight)
               ! A.rx (i cornerRadius) ! A.ry (i cornerRadius)
    S.g ! A.style "clip-path: url(#clipPath)" $ do
      S.g $ do
        S.path ! A.fill "#555" ! A.d leftPath
        S.path ! A.fill "#e25" ! A.d rightPath
      S.g ! A.fill "#fff" ! A.textAnchor "middle" ! A.fontSize "12" ! A.fontFamily "sans-serif" $ do
        leftText   "pursuit"
        rightText  ('v' : showVersion version)
  where
  i' :: Int -> String
  i' = show

  i = toValue . i'

  totalWidth = 110
  totalHeight = 20
  partitionX = 50
  partitionY = totalWidth - partitionX
  cornerRadius = 3

  leftPath = toValue $
    concat ["M0 0 h", i' partitionX, " v", i' totalHeight, " H0 z"]

  rightPath = toValue $
    concat ["M", i' partitionX, " 0 h", i' partitionY, " v", i' totalHeight, " H", i' partitionX, " z"]

  textAt x y str =
    S.text_ ! A.x (i x) ! A.y (i y) ! A.dy ".3em" $ S.text $ pack str

  leftText  = textAt (div partitionX 2)                               (div totalHeight 2)
  rightText = textAt (partitionX + (div (totalWidth - partitionX) 2)) (div totalHeight 2)
