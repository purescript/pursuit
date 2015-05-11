
module Handler.Caching
  ( cache
  , clearCache
  ) where

import Import
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Lazy as LT
import Text.Blaze.Html (preEscapedToHtml)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import System.Directory (removeDirectoryRecursive, createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Web.Bower.PackageMeta (PackageName, runPackageName)

import Handler.Utils

type CacheKey = (PackageName, String)

-- | Cache an expensive bit of html under a specific key.
cache :: CacheKey -> Handler Html -> Handler Html
cache key innerAction = do
  file <- cacheFileFor key
  mcontents <- liftIO (readFileMay file :: IO (Maybe LT.Text))
  case mcontents of
    Just contents -> do
      $logDebug "Cache hit"
      return (preEscapedToHtml contents)
    Nothing -> do
      $logDebug "Cache miss"
      html <- innerAction
      liftIO $ do
        createDirectoryIfMissing True (takeDirectory file)
        LB.writeFile file (renderHtml html)
      return html

-- | Clear the whole cache for a particular package
clearCache :: PackageName -> Handler ()
clearCache pkgName = do
  dir <- cacheDirFor pkgName
  $logDebug ("clearing cache for: " ++ pack (runPackageName pkgName))
  liftIO $ removeDirectoryRecursive dir

cacheDirFor :: PackageName -> Handler String
cacheDirFor pkgName = do
  dataDir <- getDataDir
  return (dataDir ++ "/cache/" ++ runPackageName pkgName)

cacheFileFor :: CacheKey -> Handler String
cacheFileFor (pkgName, path) = do
  dir <- cacheDirFor pkgName
  return $ dir ++ "/" ++ path ++ ".html"
