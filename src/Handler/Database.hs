
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
  lookupPackageMay = map hush . uncurry lookupPackage

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
  return . fromListWithDuplicates $
    primEntries ++ concatMap entriesForPackage pkgs

primEntries :: [(ByteString, (SearchResult, Maybe P.Type))]
primEntries =
  let
    mkResult = SearchResult SourceBuiltin
  in
    entriesForModule mkResult D.primDocsModule

entriesForPackage ::
  D.Package a ->
  [(ByteString, (SearchResult, Maybe P.Type))]
entriesForPackage D.Package{..} =
  let
    mkResult =
      SearchResult (SourcePackage (bowerName pkgMeta) pkgVersion)
    packageEntry =
      ( fromText (tryStripPrefix "purescript-" (T.toLower (runPackageName (bowerName pkgMeta))))
      , ( mkResult (fromMaybe "" (bowerDescription pkgMeta))
                   PackageResult
        , Nothing
        )
      )
  in
    packageEntry : concatMap (entriesForModule mkResult) pkgModules

entriesForModule ::
  (Text -> SearchResultInfo -> SearchResult) ->
  D.Module ->
  [(ByteString, (SearchResult, Maybe P.Type))]
entriesForModule mkResult D.Module{..} =
  let
    moduleEntry =
      ( fromText (T.toLower (P.runModuleName modName))
      , ( mkResult (fromMaybe "" modComments)
                   (ModuleResult (P.runModuleName modName))
        , Nothing
        )
      )
  in
    moduleEntry :
      concatMap (entriesForDeclaration mkResult modName) modDeclarations

entriesForDeclaration ::
  (Text -> SearchResultInfo -> SearchResult) ->
  P.ModuleName ->
  D.Declaration ->
  [(ByteString, (SearchResult, Maybe P.Type))]
entriesForDeclaration mkResult modName D.Declaration{..} =
  let
    ty =
      case declInfo of
        D.ValueDeclaration t -> Just t
        _ -> Nothing
    ns =
      D.declInfoNamespace declInfo
    declEntry =
      ( fromText (T.toLower declTitle)
      , ( mkResult (fromMaybe "" declComments)
                   (DeclarationResult
                      ns
                      (P.runModuleName modName)
                      declTitle
                      (fmap typeToText ty))
        , ty
        )
      )
  in
    declEntry : do
      D.ChildDeclaration{..} <- declChildren
      let ty' = extractChildDeclarationType declTitle declInfo cdeclInfo
      return ( fromText (T.toLower cdeclTitle)
             , ( mkResult (fromMaybe "" cdeclComments)
                          (DeclarationResult
                              ValueLevel
                              (P.runModuleName modName)
                              cdeclTitle
                              (fmap typeToText ty'))
               , ty'
               )
             )

typeToText :: P.Type -> Text
typeToText = outputWith renderText . renderType

fromListWithDuplicates :: [(ByteString, a)] -> Trie.Trie [a]
fromListWithDuplicates = foldr go Trie.empty
  where
  go (k, a) = Trie.alterBy (\_ xs -> Just . maybe xs (xs <>)) k [a]

-- Extract the type of a child declaration when considering it as a standalone
-- declaration. For instance, type class members need to have the appropriate
-- constraint added, and data constructors need to have their arguments plus
-- the parent data type put together to form the constructor's type.
--
-- TODO: Move this into the purescript library?
extractChildDeclarationType :: Text -> D.DeclarationInfo -> D.ChildDeclarationInfo -> Maybe P.Type
extractChildDeclarationType declTitle declInfo cdeclInfo =
  case (declInfo, cdeclInfo) of
    (D.TypeClassDeclaration args _ _ , D.ChildTypeClassMember ty) ->
      let
        constraint =
          P.Constraint
            { P.constraintClass = parentName
            , P.constraintArgs = map (P.TypeVar . fst) args
            , P.constraintData = Nothing
            }
        in
          Just (addConstraint constraint ty)
    (D.DataDeclaration _ tyArgs, D.ChildDataConstructor args) ->
      let
        dataTy = foldl' P.TypeApp (P.TypeConstructor parentName)
                                  (map (P.TypeVar . fst) tyArgs)
        mkFun t1 t2 = P.TypeApp (P.TypeApp P.tyFunction t1) t2
      in
        Just . P.quantify $ case args of
          [] ->
            dataTy
          (a:as) ->
            foldl' mkFun a (as ++ [dataTy])
    _ ->
      Nothing

  where
    parentName :: P.Qualified (P.ProperName a)
    parentName = P.Qualified Nothing (P.ProperName declTitle)

    addConstraint constraint =
      P.quantify . P.moveQuantifiersToFront . P.ConstrainedType constraint

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
  writeFileWithParents file (toStrict (A.encode pkg))

packageDirFor :: PackageName -> Handler String
packageDirFor pkgName = do
  dir <- getDataDir
  return (dir ++ "/verified/" ++ unpack (runPackageName pkgName))

packageVersionFileFor :: PackageName -> Version -> Handler String
packageVersionFileFor pkgName version = do
  dir <- packageDirFor pkgName
  return (dir ++ "/" ++ showVersion version ++ ".json")

decodeVerifiedPackageFile :: String -> ByteString -> Handler D.VerifiedPackage
decodeVerifiedPackageFile filepath contents =
  decodePackageFile filepath contents

-- | Prefer decodeVerifiedPackageFile to this function, where possible.
decodePackageFile :: (A.FromJSON a) => String -> ByteString -> Handler (D.Package a)
decodePackageFile filepath contents = do
  case A.eitherDecodeStrict contents of
    Left err -> do
      $logError (T.pack ("Invalid JSON in: " ++ show filepath ++
                         ", error: " ++ show err))
      sendResponseStatus internalServerError500 ("" :: String)
    Right pkg ->
      return pkg
