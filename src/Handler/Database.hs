
module Handler.Database
  ( createDatabase
  , getAllPackageNames
  , getAllPackages
  , getLatestPackages
  , lookupPackage
  , availableVersionsFor
  , getLatestVersionFor
  , insertPackage
  , SomethingMissing(..)
  ) where

import Import
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.NonNull as NN
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Trie as Trie
import Data.Version (Version, showVersion)
import System.Directory (getDirectoryContents, getModificationTime, doesDirectoryExist)

import Web.Bower.PackageMeta (PackageName, bowerName, bowerDescription,
                              mkPackageName, runPackageName)
import qualified Language.PureScript as P
import qualified Language.PureScript.Docs as D
import Language.PureScript.Docs.RenderedCode (renderType, RenderedCodeElement(..), Namespace(..), outputWith)

import Handler.Utils
import Handler.Caching (clearCache)

getAllPackageNames :: Handler [PackageName]
getAllPackageNames = do
  dir <- getDataDir
  contents <- liftIO $ getDirectoryContents (dir ++ "/verified/")
  return . sort . rights $ map (mkPackageName . pack) contents

getLatestPackages :: Handler [(PackageName, Version)]
getLatestPackages = do
    pkgNames <- getAllPackageNames
    pkgNamesAndTimestamps <- traverse withTimestamp pkgNames
    let latest = (map fst . take 10 . sortBy (comparing (Down . snd))) pkgNamesAndTimestamps
    catMaybes <$> traverse withVersion latest
  where
    withTimestamp :: PackageName -> Handler (PackageName, UTCTime)
    withTimestamp name = map (name,) (getPackageModificationTime name)

    withVersion :: PackageName -> Handler (Maybe (PackageName, Version))
    withVersion name = (map . map) (name,) (getLatestVersionFor name)

-- | This is horribly inefficient, but it will do for now.
getAllPackages :: Handler [D.VerifiedPackage]
getAllPackages = do
  pkgNames <- getAllPackageNames
  pkgNamesAndVersions <- catMaybes <$> traverse withVersion pkgNames
  catMaybes <$> traverse lookupPackageMay pkgNamesAndVersions
  where
  withVersion name = (map . map) (name,) (getLatestVersionFor name)
  lookupPackageMay = map (either (const Nothing) Just) . uncurry lookupPackage

tryStripPrefix :: Text -> Text -> Text
tryStripPrefix pre s = fromMaybe s (T.stripPrefix pre s)

renderText :: RenderedCodeElement -> Text
renderText str = case str of
  Syntax s -> s
  Keyword s -> s
  Space -> " "
  Symbol _ s _ -> s

fromText :: Text -> ByteString
fromText = TE.encodeUtf8

createDatabase :: Handler (Trie.Trie [(SearchResult, Maybe P.Type)])
createDatabase = do
  pkgs <- getAllPackages
  return . fromListWithDuplicates $ do
    D.Package{..} <- pkgs
    let packageEntry =
          ( fromText (tryStripPrefix "purescript-" (T.toLower (runPackageName (bowerName pkgMeta))))
          , ( SearchResult (bowerName pkgMeta)
                         pkgVersion
                         (fromMaybe "" (bowerDescription pkgMeta))
                         PackageResult
            , Nothing
            )
          )
    packageEntry : do
      D.Module{..} <- pkgModules
      let moduleEntry =
            ( fromText (T.toLower (P.runModuleName modName))
            , ( SearchResult (bowerName pkgMeta)
                             pkgVersion
                             (fromMaybe "" modComments)
                             (ModuleResult (P.runModuleName modName))
              , Nothing
              )
            )
      moduleEntry : do
        D.Declaration{..} <- modDeclarations
        let ty = case declInfo of
                    D.ValueDeclaration t -> Just t
                    _ -> Nothing
            ns = D.declInfoNamespace declInfo
            declEntry =
               ( fromText (T.toLower declTitle)
               , ( SearchResult (bowerName pkgMeta)
                                pkgVersion
                                (fromMaybe "" declComments)
                                (DeclarationResult ns (P.runModuleName modName) declTitle (fmap (outputWith renderText . renderType) ty))
                 , ty
                 )
               )
        declEntry : do
          D.ChildDeclaration{..} <- declChildren
          return ( fromText (T.toLower cdeclTitle)
                 , ( SearchResult (bowerName pkgMeta)
                                  pkgVersion
                                  (fromMaybe "" cdeclComments)
                                  (DeclarationResult ValueLevel (P.runModuleName modName) cdeclTitle Nothing)
                   , Nothing
                   )
                 )
  where
    fromListWithDuplicates :: [(ByteString, a)] -> Trie.Trie [a]
    fromListWithDuplicates = foldr (\(k, a) -> Trie.alterBy (\_ xs -> Just . maybe xs (xs <>)) k [a]) Trie.empty

data SomethingMissing
  = NoSuchPackage
  | NoSuchPackageVersion
  deriving (Show, Eq, Ord)

lookupPackage :: PackageName -> Version -> Handler (Either SomethingMissing D.VerifiedPackage)
lookupPackage pkgName version = do
  file <- packageVersionFileFor pkgName version
  mcontents <- liftIO (readFileMay file)
  case mcontents of
    Just contents ->
      Right <$> decodeVerifiedPackageFile file contents
    Nothing -> do
      -- Work out whether there's no such package or just no such version
      dir <- packageDirFor pkgName
      exists <- liftIO $ doesDirectoryExist dir
      return $ Left $ if exists then NoSuchPackageVersion else NoSuchPackage

availableVersionsFor :: PackageName -> Handler [Version]
availableVersionsFor pkgName = do
  dir <- packageDirFor pkgName
  mresult <- liftIO $ catchDoesNotExist $ do
    files <- getDirectoryContents dir
    return $ mapMaybe (stripSuffix ".json" >=> D.parseVersion') files
  return $ fromMaybe [] mresult

getPackageModificationTime :: PackageName -> Handler UTCTime
getPackageModificationTime pkgName = do
  dir <- packageDirFor pkgName
  liftIO $ getModificationTime dir

getLatestVersionFor :: PackageName -> Handler (Maybe Version)
getLatestVersionFor pkgName = do
  vs  <- availableVersionsFor pkgName
  return $ map NN.maximum (NN.fromNullable vs)

-- | Insert a package at a specific version into the database.
insertPackage :: D.VerifiedPackage -> Handler ()
insertPackage pkg@D.Package{..} = do
  let pkgName = D.packageName pkg
  file <- packageVersionFileFor pkgName pkgVersion
  clearCache pkgName pkgVersion
  writeFileWithParents file (A.encode pkg)

packageDirFor :: PackageName -> Handler String
packageDirFor pkgName = do
  dir <- getDataDir
  return (dir ++ "/verified/" ++ unpack (runPackageName pkgName))

packageVersionFileFor :: PackageName -> Version -> Handler String
packageVersionFileFor pkgName version = do
  dir <- packageDirFor pkgName
  return (dir ++ "/" ++ showVersion version ++ ".json")

decodeVerifiedPackageFile :: String -> BL.ByteString -> Handler D.VerifiedPackage
decodeVerifiedPackageFile filepath contents =
  decodePackageFile filepath contents

-- | Prefer decodeVerifiedPackageFile to this function, where possible.
decodePackageFile :: (A.FromJSON a) => String -> BL.ByteString -> Handler (D.Package a)
decodePackageFile filepath contents = do
  case A.eitherDecode contents of
    Left err -> do
      $logError (T.pack ("Invalid JSON in: " ++ show filepath ++
                         ", error: " ++ show err))
      sendResponseStatus internalServerError500 ("" :: String)
    Right pkg ->
      return pkg
