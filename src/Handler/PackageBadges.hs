
module Handler.PackageBadges where

import Import
import Data.Version
import qualified Text.Blaze as Blaze
import qualified Text.Blaze.Svg11 as S
import Text.Blaze.Svg.Renderer.Text (renderSvg)
import qualified Graphics.Badge.Barrier as Badge

import Handler.Database (getLatestVersionFor)
import Handler.Caching (cacheSvg)

newtype ContentSvg = ContentSvg { runContentSvg :: S.Svg }

instance ToContent ContentSvg where
  toContent = toContent . renderSvg . runContentSvg

instance ToTypedContent ContentSvg where
  toTypedContent = TypedContent typeSvg . toContent

getPackageBadgeR :: PathPackageName -> Handler ContentSvg
getPackageBadgeR (PathPackageName pkgName) =
  map ContentSvg $ cacheSvg $ do
    msvg <- (map . map) renderBadge (getLatestVersionFor pkgName)
    case msvg of
      Just svg -> return svg
      Nothing  -> notFound

renderBadge :: Version -> S.Svg
renderBadge version =
  Blaze.unsafeLazyByteString (Badge.renderBadge badge left right)
  where
  badge = Badge.flat
  left = "pursuit"
  right = pack ('v' : showVersion version)
