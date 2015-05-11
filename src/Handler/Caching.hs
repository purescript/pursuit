
module Handler.Caching
  ( cache
  , cacheMay
  , clearCache
  ) where

import Import
import Data.Version (Version, showVersion)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Lazy as LT
import Text.Blaze.Html (preEscapedToHtml)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import System.Directory (removeDirectoryRecursive, createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Web.Bower.PackageMeta (PackageName, runPackageName)

import Handler.Utils

type CacheKey = (PackageName, Version, String)

-- | Cache an expensive bit of html under a specific key. It is the caller's
-- responsibility to ensure that cache keys do not overlap.
--
-- Whenever a new version of a package is uploaded, the whole cache for that
-- package at that version is wiped out.
cache :: CacheKey -> Handler Html -> Handler Html
cache = cache' onMiss fromLazyText
  where
  onMiss file html = do
    liftIO $ do
      createDirectoryIfMissing True (takeDirectory file)
      LB.writeFile file (renderHtml html)
    return html

  fromLazyText = preEscapedToHtml

-- | Cache an expensive bit of html under a specific key, for a generation
-- action which might fail (such as getting the README from GitHub). In this
-- case, the result is only cached if a Just value is returned from the action.
cacheMay :: CacheKey -> Handler (Maybe Html) -> Handler (Maybe Html)
cacheMay = cache' onMiss fromLazyText
  where
  onMiss _ Nothing =
    return Nothing
  onMiss file (Just html) = do
    liftIO $ do
      createDirectoryIfMissing True (takeDirectory file)
      LB.writeFile file (renderHtml html)
    return (Just html)

  fromLazyText = Just . preEscapedToHtml

-- | Helper function for cache and cacheMay
cache' :: (String -> a -> Handler a) -> (LT.Text -> a) -> CacheKey -> Handler a -> Handler a
cache' onMiss fromLazyText key innerAction = do
  file <- cacheFileFor key
  mcontents <- liftIO (readFileMay file :: IO (Maybe LT.Text))
  case mcontents of
    Just contents -> do
      $logDebug ("Cache hit: " ++ keyToText key)
      return $ fromLazyText contents
    Nothing -> do
      $logDebug ("Cache miss: " ++ keyToText key)
      innerAction >>= onMiss file

-- | Clear the whole cache for a particular package
clearCache :: PackageName -> Version -> Handler ()
clearCache pkgName version = do
  dir <- cacheDirFor pkgName version
  $logDebug ("clearing cache for: " ++ pack (runPackageName pkgName))
  liftIO $ removeDirectoryRecursive dir

cacheDirFor :: PackageName -> Version -> Handler String
cacheDirFor pkgName version = do
  dataDir <- getDataDir
  return (dataDir ++ "/cache/" ++
          runPackageName pkgName ++ "/" ++
          showVersion version)

cacheFileFor :: CacheKey -> Handler String
cacheFileFor (pkgName, version, path) = do
  dir <- cacheDirFor pkgName version
  return $ dir ++ "/" ++ path ++ ".html"

keyToText :: CacheKey -> Text
keyToText (pkgName, version, path) =
  pack (intercalate ":" [runPackageName pkgName, showVersion version, path])
