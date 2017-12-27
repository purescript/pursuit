module SearchIndex
  ( SearchResult(..)
  , SearchResultSource(..)
  , SearchResultInfo(..)
  , SearchIndex
  , emptySearchIndex
  , createSearchIndex
  , evalSearchIndex
  , searchForName
  , searchForType
  , compareTypes
  , typeComplexity
  , parseType
  , isSymbol
  ) where

import Import.NoFoundation
import Control.Parallel.Strategies (Strategy, evalTraversable, rdeepseq)
import Data.Trie (Trie)
import Data.Version (Version)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Trie as Trie
import qualified Text.Parsec.Combinator as Parsec
import qualified Language.PureScript as P
import qualified Language.PureScript.Docs as D
import Web.Bower.PackageMeta
  (PackageName, bowerName, bowerDescription, runPackageName)

-- | A single search result.
data SearchResult = SearchResult
  { srSource   :: SearchResultSource
  , srComments :: Text
  , srInfo     :: SearchResultInfo
  }
  deriving (Show, Eq, Generic)

instance NFData SearchResult

-- | Tells you where a search result came from.
data SearchResultSource
  = SourceBuiltin
  | SourcePackage PackageName Version
  deriving (Show, Eq, Generic)

instance NFData SearchResultSource

data SearchResultInfo
  = PackageResult
  | ModuleResult Text
  -- ^ Module name
  | DeclarationResult D.Namespace Text Text (Maybe Text)
  -- ^ Module name & declaration title & type if value
  deriving (Show, Eq, Generic)

instance NFData SearchResultInfo

instance ToJSON SearchResultInfo where
  toJSON i = object $ case i of
    PackageResult ->
      [ "type" .= ("package" :: Text)
      ]
    ModuleResult moduleName ->
      [ "type" .= ("module" :: Text)
      , "module" .= moduleName
      ]
    DeclarationResult typeOrValue moduleName declTitle typeText ->
      [ "type" .= ("declaration" :: Text)
      , "typeOrValue" .= show typeOrValue
      , "module" .= moduleName
      , "title" .= declTitle
      , "typeText" .= typeText
      ]

newtype SearchIndex
  = SearchIndex { unSearchIndex :: Trie [(SearchResult, Maybe P.Type)] }

emptySearchIndex :: SearchIndex
emptySearchIndex = SearchIndex Trie.empty

-- | Given a list of packages, create a search index for them.
createSearchIndex :: [D.Package a] -> SearchIndex
createSearchIndex =
  countReverseDependencies
  >>> sortWith (Down . snd)
  >>> map fst
  >>> concatMap entriesForPackage
  >>> (primEntries ++)
  >>> fromListWithDuplicates
  >>> SearchIndex

-- | A strategy for evaluating a SearchIndex in parallel.
evalSearchIndex :: Strategy SearchIndex
evalSearchIndex = fmap SearchIndex . evalTraversable rdeepseq . unSearchIndex

-- |
-- Given a list of packages (which should not include duplicates, or more than
-- one version of any given package), return a list of packages together with
-- the number of reverse dependencies each one has, in no particular order.
--
countReverseDependencies :: [D.Package a] -> [(D.Package a, Int)]
countReverseDependencies packages =
  Map.elems $ foldl' go initialMap packages
  where
  initialMap =
    Map.fromList $ map (\pkg -> (D.packageName pkg, (pkg, 0))) packages

  go m pkg =
    foldl' (flip increment) m
      (map fst (D.pkgResolvedDependencies pkg))

  increment =
    Map.adjust (second (+1))

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
      ( encodeUtf8 (tryStripPrefix "purescript-" (T.toLower (runPackageName (bowerName pkgMeta))))
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
      ( encodeUtf8 (T.toLower (P.runModuleName modName))
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
      ( encodeUtf8 (T.toLower (handleTypeOp declTitle))
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
      return ( encodeUtf8 (T.toLower cdeclTitle)
             , ( mkResult (fromMaybe "" cdeclComments)
                          (DeclarationResult
                              D.ValueLevel
                              (P.runModuleName modName)
                              cdeclTitle
                              (fmap typeToText ty'))
               , ty'
               )
             )
  where
  -- The declaration title of a type operator is e.g. "type (/\)". Here we
  -- remove this prefix but leave other kinds of declarations unchanged.
  handleTypeOp = tryStripPrefix "type "

typeToText :: P.Type -> Text
typeToText = D.outputWith renderText . D.renderType


renderText :: D.RenderedCodeElement -> Text
renderText codeElem = case codeElem of
  D.Syntax s -> s
  D.Keyword s -> s
  D.Space -> " "
  D.Symbol _ s _ -> s

fromListWithDuplicates :: [(ByteString, a)] -> Trie [a]
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

-- | Return the entire list of entries which match the given query, together
-- with a nonnegative score (lower is better).
searchForName :: Text -> SearchIndex -> [(SearchResult, Int)]
searchForName query =
  let
    query' =
      toLower $
        if isSymbol query
          then "(" <> query
          else tryStripPrefix "purescript-" query
    convert (key, rs) =
      -- note that, because we are using a trie here, all the results are at
      -- least as long as the query; we use the difference in length as the
      -- score.
      map (\r -> (fst r, T.length (decodeUtf8 key) - T.length query')) rs
  in
    unSearchIndex
    >>> Trie.submap (encodeUtf8 query')
    >>> Trie.toList
    >>> map convert
    >>> concat
    >>> sortWith snd

-- | Search the index by type. If the query does not parse as a type, or is a
-- "simple type", i.e. just a single type constructor or type variable, return
-- an empty list. Returns a list of (result, score) where the score is
-- nonnegative and lower is better.
--
-- We exclude simple types because they will be picked up by name-based search.
searchForType :: Text -> SearchIndex -> [(SearchResult, Int)]
searchForType query =
  case parseType query of
    Just ty | not (isSimpleType ty) ->
      searchForType' ty
    _ ->
      const []
  where
  isSimpleType :: P.Type -> Bool
  isSimpleType P.TypeVar{} = True
  isSimpleType P.TypeConstructor{} = True
  isSimpleType _ = False

searchForType' :: P.Type -> SearchIndex -> [(SearchResult, Int)]
searchForType' ty =
  unSearchIndex
  >>> Trie.elems
  >>> concat
  >>> mapMaybe (matches ty)
  >>> sortWith snd
  where
  matches :: P.Type -> (a, Maybe P.Type) -> Maybe (a, Int)
  matches ty1 (a, Just ty2) = do
    score <- compareTypes ty1 ty2
    return (a, score)
  matches _ _ = Nothing

tryStripPrefix :: Text -> Text -> Text
tryStripPrefix pre s = fromMaybe s (T.stripPrefix pre s)

-- | This is an approximation to type subsumption / unification. This function
-- returns Just a score if there is a possible match, or Nothing otherwise.
-- Lower scores are better.
--
-- The first argument is the query, and the second is the candidate result.
-- This function is not symmetric; for example:
--
-- let compare s1 s2 = compareTypes <$> parseType s2 <*> parseType s2
--
-- >>> compare "a" "Int"
-- Just Nothing
-- >>> compare "Int" "a"
-- Just (Just 1)
--
compareTypes :: P.Type -> P.Type -> Maybe Int
compareTypes (P.TypeVar _) (P.TypeVar _) = Just 0
compareTypes t (P.TypeVar _) = Just (1 + typeComplexity t)
compareTypes (P.TypeLevelString s1) (P.TypeLevelString s2) | s1 == s2 = Just 0
compareTypes (P.TypeWildcard _) t = Just (typeComplexity t)
compareTypes (P.TypeConstructor q1) (P.TypeConstructor q2) | compareQual q1 q2 = Just 0
-- There is a special case for functions, since if the user _asked_ for a function,
-- they probably don't want to see something more general of type 'f a' or 'f a b'.
compareTypes (P.TypeApp a b) (P.TypeApp c d)
  | not (isFunction a) || isFunction c = (+) <$> compareTypes a c <*> compareTypes b d
compareTypes (P.ForAll _ t1 _) t2 = compareTypes t1 t2
compareTypes t1 (P.ForAll _ t2 _) = compareTypes t1 t2
compareTypes (P.ConstrainedType _ t1) t2 = compareTypes t1 t2
compareTypes t1 (P.ConstrainedType _ t2) = compareTypes t1 t2
compareTypes P.REmpty P.REmpty = Just 0
compareTypes t1@P.RCons{} t2 = compareRows t1 t2
compareTypes t1 t2@P.RCons{} = compareRows t1 t2
compareTypes (P.KindedType t1 _) t2 = compareTypes t1 t2
compareTypes t1 (P.KindedType t2 _) = compareTypes t1 t2
-- Really, we should desugar any type operators here.
-- Since type operators are not supported in search right now, this is fine,
-- since we only care about functions, which are already in the correct
-- order as they come out of the parser.
compareTypes (P.ParensInType t1) t2 = compareTypes t1 t2
compareTypes t1 (P.ParensInType t2) = compareTypes t1 t2
compareTypes _ _ = Nothing

isFunction :: P.Type -> Bool
isFunction (P.TypeConstructor (P.Qualified _ (P.ProperName "Function"))) = True
isFunction _ = False

compareRows :: P.Type -> P.Type -> Maybe Int
compareRows r1 r2 = sum <$>
  sequence [ compareTypes t1 t2
           | (name, t1) <- fst (P.rowToList r1)
           , (name', t2) <- fst (P.rowToList r2)
           , name == name'
           ]

typeComplexity :: P.Type -> Int
typeComplexity (P.TypeApp a b) = 1 + typeComplexity a + typeComplexity b
typeComplexity (P.ForAll _ t _) = 1 + typeComplexity t
typeComplexity (P.ConstrainedType _ t) = typeComplexity t + 1
typeComplexity P.REmpty = 0
typeComplexity (P.RCons _ t r) = 1 + typeComplexity t + typeComplexity r
typeComplexity (P.KindedType t _) = typeComplexity t
typeComplexity (P.ParensInType t) = typeComplexity t
typeComplexity _ = 0

compareQual :: Eq a => P.Qualified a -> P.Qualified a -> Bool
compareQual (P.Qualified (Just mn1) a1) (P.Qualified (Just mn2) a2) = mn1 == mn2 && a1 == a2
compareQual (P.Qualified _ a1) (P.Qualified _ a2) = a1 == a2

parseWithTokenParser :: P.TokenParser a -> Text -> Maybe a
parseWithTokenParser p =
  hush . (P.lex "") >=> hush . (P.runTokenParser "" (p <* Parsec.eof))

parseType :: Text -> Maybe P.Type
parseType = parseWithTokenParser P.parsePolyType

isSymbol :: Text -> Bool
isSymbol = maybe False (const True) . parseWithTokenParser P.symbol
