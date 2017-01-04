
module Handler.Caching
  ( OkToCache(..)
  , cache
  , cacheConditional
  , cacheHtml
  , cacheHtmlConditional
  , cacheSvg
  , cacheJSON
  , cacheText
  , clearCache
  ) where

import Import
import Data.Version (Version, showVersion)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Svg11 (Svg)
import Text.Blaze.Svg.Renderer.Utf8 (renderSvg)
import System.Directory (removeDirectoryRecursive, removeFile, getDirectoryContents)
import Web.Bower.PackageMeta (PackageName, runPackageName)
import Data.Aeson (encode)

import Handler.Utils

data OkToCache
  = OkToCache
  | NotOkToCache
  deriving (Show, Eq, Ord)

-- | This function allows an upstream server (such as nginx) to cache a
-- response, by writing the response body to a file before sending it to the
-- client. Afterwards, the upstream server can respond to requests for the same
-- path without having to proxy to the Yesod application, and any caching
-- mechanisms that the upstream server supports can additionally be used.
--
-- We use Yesod's short-circuiting behaviour here to ensure that this code is
-- never reached if the inner handler generates an internal server error, or a
-- client error such as a 404 (in which case the response should not be
-- cached).
cacheConditional :: (a -> LB.ByteString) -> String -> Handler (OkToCache, a) -> Handler a
cacheConditional toLbs basename action = do
  (status, body) <- action
  case status of
    OkToCache    -> write body
    NotOkToCache -> $logDebug "response is not cacheable, writing skipped"
  return body
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

-- | A variant of cache' to be used when the response is always cacheable
-- (assuming the inner handler completes and returns a value).
cache :: (a -> LB.ByteString) -> String -> Handler a -> Handler a
cache toLbs basename action =
  cacheConditional toLbs basename ((OkToCache,) <$> action)

cacheHtml :: Handler Html -> Handler Html
cacheHtml = cache renderHtml "index.html"

cacheHtmlConditional :: Handler (OkToCache, Html) -> Handler Html
cacheHtmlConditional = cacheConditional renderHtml "index.html"

cacheSvg :: Handler Svg -> Handler Svg
cacheSvg = cache renderSvg "index.svg"

cacheJSON :: Handler Value -> Handler Value
cacheJSON = cache encode "index.json"

cacheText :: Handler LT.Text -> Handler LT.Text
cacheText = cache LTE.encodeUtf8 "index.txt"

-- | Clear the whole cache for a particular package at a particular version.
-- Called whenever a new version of a package is uploaded.
--
-- Note that this function is also responsible for clearing cached
-- resources that are not associated with any particular version, but which
-- need to be regenerated after a new version is uploaded. This includes the
-- available-versions JSON object, or the SVG badge.
clearCache :: PackageName -> Version -> Handler ()
clearCache pkgName version = do
  $logDebug ("clearing cache for: " <> runPackageName pkgName <>
                    ", at version: " <> pack (showVersion version))

  -- TODO: hack, this should be improved. Not quite sure how, though.
  removeSpecific
  removeShared

  where
  -- Remove files specific to that package.
  removeSpecific =
    let
      pkgName' = PathPackageName pkgName
    in
      eachRouteDir
        [ PackageVersionR pkgName' (PathVersion version)
        , PackageAvailableVersionsR pkgName'
        , PackageBadgeR pkgName'
        ]
        removeDirectoryRecursive

  -- Remove files that need to be regenerated every time a new package is
  -- uploaded, eg lists of all packages.
  removeShared =
    eachRouteDir
      [ PackageIndexR
      , HomeR
      ]
      removeIndexFiles

  eachRouteDir routes f = do
    dirs <- getRouteCacheDirs routes
    forM_ dirs (liftIO . void . catchDoesNotExist . f)

  -- Remove all files that start "index." in a directory.
  removeIndexFiles :: FilePath -> IO ()
  removeIndexFiles path = do
    files <- filter ("index." `isPrefixOf`) <$> getDirectoryContents path
    putStrLn ("Removing index files at: " <> tshow path <> ", files:" <> tshow files)
    mapM_ (void . catchDoesNotExist . removeFile . (path ++)) files


getCacheDir :: Handler String
getCacheDir = (++ "/cache/") <$> getDataDir

getRouteCacheDir :: Route App -> Handler String
getRouteCacheDir route =
  getCacheDir <#> flip cachePathFor route
  where
  (<#>) = flip (<$>)

getRouteCacheDirs :: [Route App] -> Handler [String]
getRouteCacheDirs routes =
  getCacheDir <#> (\dir -> map (cachePathFor dir) routes)
  where
  (<#>) = flip (<$>)

cachePathFor :: String -> Route App -> String
cachePathFor cacheDir =
  renderRoute
  >>> fst
  >>> intercalate "/"
  >>> unpack
  >>> (cacheDir ++)
  >>> (++ "/")
