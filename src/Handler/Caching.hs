
module Handler.Caching
  ( cache
  , cacheHtml
  , cacheSvg
  , cacheJSON
  , cacheText
  , clearCache
  ) where

import Import
import Control.Category ((>>>))
import Data.Version (Version, showVersion)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Svg11 (Svg)
import Text.Blaze.Svg.Renderer.Utf8 (renderSvg)
import System.Directory (removeDirectoryRecursive)
import Web.Bower.PackageMeta (PackageName, runPackageName)
import Data.Aeson (encode)

import Handler.Utils

-- | This function allows an upstream server (such as nginx) to cache a
-- response, by writing the response body to a file before sending it to the
-- client. Afterwards, the upstream server can respond to requests for the same
-- path without having to proxy to the Yesod application, and any caching
-- mechanisms that the upstream server supports can additionally be used.
cache :: (a -> LB.ByteString) -> String -> Handler a -> Handler a
cache toLbs basename action = action >>= (\body -> write body $> body)
  where
  write body = do
    mroute <- getCurrentRoute
    case mroute of
      Nothing -> return ()
      Just route -> do
        dir <- getRouteCacheDir route
        let path = dir ++ basename
        $logDebug ("writing response to disk for caching: " ++ pack path)
        writeFileWithParents path (toLbs body)

cacheHtml :: Handler Html -> Handler Html
cacheHtml = cache renderHtml "index.html"

cacheSvg :: Handler Svg -> Handler Svg
cacheSvg = cache renderSvg "index.svg"

cacheJSON :: Handler Value -> Handler Value
cacheJSON = cache encode "index.json"

cacheText :: Handler LT.Text -> Handler LT.Text
cacheText = cache LTE.encodeUtf8 "index.txt"

-- | Clear the whole cache for a particular package at a particular version.
-- Called whenever a new version of a package is uploaded.
clearCache :: PackageName -> Version -> Handler ()
clearCache pkgName version = do
  $logDebug (pack $ "clearing cache for: " ++ runPackageName pkgName ++
                    ", at version: " ++ showVersion version)

  dir  <- getRouteCacheDir (PackageVersionR (PathPackageName pkgName) (PathVersion version))
  dir2 <- getRouteCacheDir (PackageAvailableVersionsR (PathPackageName pkgName))
  forM_ [dir, dir2] (liftIO . void . catchDoesNotExist . removeDirectoryRecursive)

getCacheDir :: Handler String
getCacheDir = (++ "/cache/") <$> getDataDir

getRouteCacheDir :: Route App -> Handler String
getRouteCacheDir route = go <$> getCacheDir <*> pure route
  where
  go cacheDir =
    renderRoute
    >>> fst
    >>> intercalate "/"
    >>> unpack
    >>> (cacheDir ++)
    >>> (++ "/")
