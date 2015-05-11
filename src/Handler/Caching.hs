
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

-- | A key that identifies one cached chunk of HTML. Most cached chunks are
-- associated with a particular package at a particular version, which the
-- first two elements represent. If the version is Nothing, then this means
-- the chunk applies to all versions; for example, the version selector is
-- cached in this way.
type CacheKey = (PackageName, Maybe Version, String)

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

-- | Clear the whole cache for a particular package at a particular version
clearCache :: PackageName -> Version -> Handler ()
clearCache pkgName version = do
  dir  <- cacheDirFor pkgName (Just version)
  dir2 <- cacheDirFor pkgName Nothing
  $logDebug ("clearing cache for: " ++ pack (runPackageName pkgName))
  forM_ [dir, dir2] (liftIO . void . catchDoesNotExist . removeDirectoryRecursive)

cacheDirFor :: PackageName -> Maybe Version -> Handler String
cacheDirFor pkgName mversion = do
  dataDir <- getDataDir
  return (dataDir ++ "/cache/" ++
          runPackageName pkgName ++ "/" ++
          maybe "all" showVersion mversion)

cacheFileFor :: CacheKey -> Handler String
cacheFileFor (pkgName, version, path) = do
  dir <- cacheDirFor pkgName version
  return $ dir ++ "/" ++ path ++ ".html"

keyToText :: CacheKey -> Text
keyToText (pkgName, mversion, path) =
  let version = maybe "all" showVersion mversion
  in  pack (intercalate ":" [runPackageName pkgName, version, path])
