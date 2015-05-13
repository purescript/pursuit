
module Handler.PackageBadges where

import Import
import Data.Version
import Text.Blaze (toValue)
import Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.Text (renderSvg)

import Handler.Packages (getLatestVersion)

getPackageBadgeR :: PathPackageName -> Handler TypedContent
getPackageBadgeR (PathPackageName pkgName) = do
  v <- getLatestVersion pkgName
  case v of
    Nothing ->
      notFound
    Just v' ->
      let content = toContent $ renderSvg $ renderBadge v'
      in  return $ TypedContent typeSvg content

renderBadge :: Version -> S.Svg
renderBadge version =
  S.docTypeSvg ! A.version "1.1" ! A.width (i totalWidth) ! A.height (i totalHeight) $ do
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

  leftPath = toValue $
    concat ["M0 0 h", i' partitionX, " v", i' totalHeight, " H0 z"]

  rightPath = toValue $
    concat ["M", i' partitionX, " 0 h", i' partitionY, " v", i' totalHeight, " H", i' partitionX, " z"]

  textAt x y str =
    S.text_ ! A.x (i x) ! A.y (i y) ! A.dy ".3em" $ S.text $ pack str

  leftText  = textAt (div partitionX 2)                               (div totalHeight 2)
  rightText = textAt (partitionX + (div (totalWidth - partitionX) 2)) (div totalHeight 2)
