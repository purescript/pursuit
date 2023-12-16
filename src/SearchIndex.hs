module SearchIndex
  ( SearchResult(..)
  , SearchResultSource(..)
  , SearchResultInfo(..)
  , searchResultTitle
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
  , isDeprecated
  ) where

import Import.NoFoundation
import Control.Parallel.Strategies (Strategy, evalTraversable, rdeepseq)
import Control.Monad.Trans.Writer (WriterT(..), tell)
import Data.Trie (Trie)
import Data.Version (Version)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Trie as Trie
import qualified Language.PureScript as P
import qualified Language.PureScript.Docs as D
import qualified Language.PureScript.CST as CST
import qualified Language.PureScript.CST.Monad as CSTM
import Web.Bower.PackageMeta
  (PackageName, bowerName, bowerDescription, bowerKeywords, runPackageName)

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
  = PackageResult Bool
  -- ^ Package deprecation status
  | ModuleResult Text
  -- ^ Module name
  | DeclarationResult D.Namespace Text Text (Maybe Text)
  -- ^ Module name & declaration title & type if value
  deriving (Show, Eq, Generic)

instance NFData SearchResultInfo

instance ToJSON SearchResultInfo where
  toJSON i = object $ case i of
    PackageResult deprecated ->
      [ "type" .= ("package" :: Text)
      , "deprecated" .= deprecated
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

searchResultTitle :: SearchResult -> Text
searchResultTitle r =
  case srInfo r of
    PackageResult _ ->
      case srSource r of
        SourceBuiltin ->
          "<builtin>"
        SourcePackage pkgName _ ->
          runPackageName pkgName
    ModuleResult modName ->
      modName
    DeclarationResult _ title _ _ ->
      title

newtype SearchIndex
  = SearchIndex { unSearchIndex :: Trie [IndexEntry] }

data IndexEntry = IndexEntry
  { entryResult  :: !SearchResult
  , entryType    :: !(Maybe D.Type')
  -- | The number of reverse dependencies of the containing package. Used for
  -- sorting otherwise equivalently-ranked results. We use 'Maybe (Down Int)'
  -- so that builtin modules (e.g. Prim) can use 'Nothing' and have it compare
  -- less than everything else, and otherwise, packages with more reverse
  -- dependencies compare less than packages with more; note that lower is
  -- better when searching.
  , entryRevDeps :: !(Maybe (Down Int))
  }
  deriving (Show, Eq, Generic)

instance NFData IndexEntry

emptySearchIndex :: SearchIndex
emptySearchIndex = SearchIndex Trie.empty

-- | Given a list of packages, create a search index for them.
createSearchIndex :: [D.Package a] -> SearchIndex
createSearchIndex =
  countReverseDependencies
  >>> sortOn (Down . snd)
  >>> concatMap (uncurry entriesForPackage)
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

primEntries :: [(ByteString, IndexEntry)]
primEntries =
  let
    mkEntry comments info mtype =
      IndexEntry
        { entryResult  = SearchResult SourceBuiltin comments info
        , entryType    = mtype
        , entryRevDeps = Nothing
        }
  in
    concatMap (entriesForModule mkEntry) D.primModules

entriesForPackage :: D.Package a -> Int -> [(ByteString, IndexEntry)]
entriesForPackage pkg@D.Package{..} revDeps =
  let
    src =
      SourcePackage (bowerName pkgMeta) pkgVersion
    mkEntry comments info mtype =
        IndexEntry
          { entryResult  = SearchResult src comments info
          , entryType    = mtype
          , entryRevDeps = Just (Down revDeps)
          }
    entryKey =
      encodeUtf8
        (tryStripPrefix "purescript-"
          (T.toLower
            (runPackageName (bowerName pkgMeta))))
    deprecated = isDeprecated pkg
    packageEntry =
      ( entryKey
      , mkEntry (fromMaybe "" (bowerDescription pkgMeta))
                (PackageResult deprecated)
                Nothing
      )
  in
    packageEntry : if deprecated
                     then []
                     else concatMap (entriesForModule mkEntry) pkgModules

entriesForModule ::
  (Text -> SearchResultInfo -> Maybe D.Type' -> IndexEntry) ->
  D.Module ->
  [(ByteString, IndexEntry)]
entriesForModule mkEntry D.Module{..} =
  let
    moduleEntry =
      ( encodeUtf8 (T.toLower (P.runModuleName modName))
      , mkEntry (fromMaybe "" modComments)
                (ModuleResult (P.runModuleName modName))
                Nothing
      )
  in
    moduleEntry :
      concatMap (entriesForDeclaration mkEntry modName) modDeclarations

entriesForDeclaration ::
  (Text -> SearchResultInfo -> Maybe D.Type' -> IndexEntry) ->
  P.ModuleName ->
  D.Declaration ->
  [(ByteString, IndexEntry)]
entriesForDeclaration mkEntry modName D.Declaration{..} =
  let
    ty =
      case declInfo of
        D.ValueDeclaration t -> Just t
        _ -> Nothing
    ns =
      D.declInfoNamespace declInfo
    declEntry =
      ( encodeUtf8 (T.toLower (handleTypeOp declTitle))
      , mkEntry (fromMaybe "" declComments)
                (DeclarationResult
                  ns
                  (P.runModuleName modName)
                  declTitle
                  (fmap typeToText ty))
                ty
      )
  in
    declEntry : do
      D.ChildDeclaration{..} <- declChildren
      let ty' = extractChildDeclarationType declTitle declInfo cdeclInfo
      return ( encodeUtf8 (T.toLower cdeclTitle)
             , mkEntry (fromMaybe "" cdeclComments)
                       (DeclarationResult
                         D.ValueLevel
                         (P.runModuleName modName)
                         cdeclTitle
                         (fmap typeToText ty'))
                       ty'
             )
  where
  -- The declaration title of a type operator is e.g. "type (/\)". Here we
  -- remove this prefix but leave other kinds of declarations unchanged.
  handleTypeOp = tryStripPrefix "type "

typeToText :: D.Type' -> Text
typeToText = D.outputWith renderText . D.renderType


renderText :: D.RenderedCodeElement -> Text
renderText codeElem = case codeElem of
  D.Syntax s -> s
  D.Keyword s -> s
  D.Space -> " "
  D.Symbol _ s _ -> s
  D.Role s -> s

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
extractChildDeclarationType :: Text -> D.DeclarationInfo -> D.ChildDeclarationInfo -> Maybe D.Type'
extractChildDeclarationType declTitle declInfo cdeclInfo =
  case (declInfo, cdeclInfo) of
    (D.TypeClassDeclaration args _ _ , D.ChildTypeClassMember ty) ->
      let
        constraint =
          P.Constraint
            { P.constraintClass = parentName
            , P.constraintArgs = map (P.TypeVar () . fst) args
            , P.constraintData = Nothing
            , P.constraintAnn = ()
            , P.constraintKindArgs = []
            }
        in
          Just (addConstraint constraint ty)
    (D.DataDeclaration _ tyArgs _, D.ChildDataConstructor args) ->
      let
        dataTy =
          foldl'
            (P.TypeApp ())
            (P.TypeConstructor () parentName)
            (map (P.TypeVar () . fst) tyArgs)
      in
        Just . P.quantify . foldr (P.TypeApp ()) dataTy $
          fmap (P.TypeApp () (P.tyFunction $> ())) args
    _ ->
      Nothing

  where
    parentName :: P.Qualified (P.ProperName a)
    parentName = P.Qualified P.ByNullSourcePos (P.ProperName declTitle)

    addConstraint constraint =
      P.quantify . P.moveQuantifiersToFront . P.ConstrainedType () constraint

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

    convert (key, entries) =
      -- note that, because we are using a trie here, all the results are at
      -- least as long as the query; we use the difference in length as the
      -- score.
      map (\entry ->
        ( entry
        , T.length (decodeUtf8 key) - T.length query'
        )) entries

  in
    unSearchIndex
    >>> Trie.submap (encodeUtf8 query')
    >>> Trie.toList
    >>> map convert
    >>> concat
    >>> sortEntries

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
  isSimpleType :: D.Type' -> Bool
  isSimpleType P.TypeVar{} = True
  isSimpleType P.TypeConstructor{} = True
  isSimpleType _ = False

searchForType' :: D.Type' -> SearchIndex -> [(SearchResult, Int)]
searchForType' ty =
  unSearchIndex
  >>> Trie.elems
  >>> concat
  >>> mapMaybe (matches ty)
  >>> sortEntries
  where
  matches :: D.Type' -> IndexEntry -> Maybe (IndexEntry, Int)
  matches ty1 entry@(IndexEntry { entryType = Just ty2 }) = do
    score <- compareTypes ty1 ty2
    return (entry, score)
  matches _ _ = Nothing

-- | Given a list of index entries and associated scores, sort them based on
-- the score followed by number of reverse dependencies, and then discard extra
-- unnecessary information, leaving only the SearchResult
sortEntries :: [(IndexEntry, Int)] -> [(SearchResult, Int)]
sortEntries =
  sortOn (\(entry, score) -> (score, entryRevDeps entry))
  >>> map (first entryResult)

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
-- Just (Just 0)
-- >>> compare "Int" "a"
-- Just (Just 1)
--
-- (The idea here being it's preferred to show a more concrete version of the
-- query, rather than showing a more general version of it.)
--
compareTypes :: D.Type' -> D.Type' -> Maybe Int
compareTypes type1 type2 =
  map calculate . runWriterT $ go type1 type2
  where
  calculate :: (Int, [(Text, Text)]) -> Int
  calculate (score, vars) = (10 * score) + typeVarPenalty vars

  go :: D.Type' -> D.Type' -> WriterT [(Text, Text)] Maybe Int
  go (P.TypeVar _ v1) (P.TypeVar _ v2) = tell [(v1, v2)] *> pure 0
  go t (P.TypeVar _ _) = pure (1 + typeComplexity t)
  go (P.TypeVar _ _) t = pure (typeComplexity t)
  go (P.TypeLevelString _ s1) (P.TypeLevelString _ s2) | s1 == s2 = pure 0
  go (P.TypeWildcard _ _) t = pure (typeComplexity t)
  go (P.TypeConstructor _ q1) (P.TypeConstructor _ q2) | compareQual q1 q2 = pure 0
-- There is a special case for functions, since if the user _asked_ for a
-- function, they probably don't want to see something more general of type 'f
-- a' or 'f a b'.
  go (P.TypeApp _ a b) (P.TypeApp _ c d)
    | not (isFunction a) || isFunction c = (+) <$> go a c <*> go b d
  go (P.ForAll _ _ _ _ t1 _) t2 = go t1 t2
  go t1 (P.ForAll _ _ _ _ t2 _) = go t1 t2
  go (P.ConstrainedType _ _ t1) t2 = go t1 t2
  go t1 (P.ConstrainedType _ _ t2) = go t1 t2
  go (P.REmpty _) (P.REmpty _) = pure 0
  go t1@P.RCons{} t2 = goRows t1 t2
  go t1 t2@P.RCons{} = goRows t1 t2
  go (P.KindedType _ t1 _) t2 = go t1 t2
  go t1 (P.KindedType _ t2 _) = go t1 t2
  -- Really, we should desugar any type operators here.
  -- Since type operators are not supported in search right now, this is fine,
  -- since we only care about functions, which are already in the correct
  -- order as they come out of the parser.
  go (P.ParensInType _ t1) t2 = go t1 t2
  go t1 (P.ParensInType _ t2) = go t1 t2
  go _ _ = lift Nothing

  goRows :: D.Type' -> D.Type' -> WriterT [(Text, Text)] Maybe Int
  goRows r1 r2 = sum <$>
    sequence [ go t1 t2
             | P.RowListItem _ name t1 <- fst (P.rowToList r1)
             , P.RowListItem _ name' t2 <- fst (P.rowToList r2)
             , name == name'
             ]

  -- Calculate a penalty based on the extent to which the type variables match.
  -- Where differences occur, those which make the result more general than the
  -- query are not penalised as harshly as those which make the result less
  -- general than the query.
  typeVarPenalty :: [(Text, Text)] -> Int
  typeVarPenalty list =
    penalty list + (3 * penalty (map swap list))
    where
    penalty =
      map (second Set.singleton)
      >>> Map.fromListWith Set.union
      >>> Map.elems
      -- If one element of the fsts is paired with more than one element of the
      -- snds, penalise based on how many more elements of the snds there are.
      >>> map (\s -> Set.size s - 1)
      >>> sum

isFunction :: D.Type' -> Bool
isFunction (P.TypeConstructor _ (P.Qualified _ (P.ProperName "Function"))) = True
isFunction _ = False


typeComplexity :: D.Type' -> Int
typeComplexity (P.TypeApp _ a b) = 1 + typeComplexity a + typeComplexity b
typeComplexity (P.ForAll _ _ _ _ t _) = 1 + typeComplexity t
typeComplexity (P.ConstrainedType _ _ t) = typeComplexity t + 1
typeComplexity (P.REmpty _) = 0
typeComplexity (P.RCons _ _ t r) = 1 + typeComplexity t + typeComplexity r
typeComplexity (P.KindedType _ t _) = typeComplexity t
typeComplexity (P.ParensInType _ t) = typeComplexity t
typeComplexity _ = 0

compareQual :: Eq a => P.Qualified a -> P.Qualified a -> Bool
compareQual (P.Qualified (P.ByModuleName mn1) a1) (P.Qualified (P.ByModuleName mn2) a2) = mn1 == mn2 && a1 == a2
compareQual (P.Qualified _ a1) (P.Qualified _ a2) = a1 == a2

runParser :: CST.Parser a -> Text -> Maybe a
runParser p =
  fmap snd
    . hush
    . CST.runTokenParser (p <* CSTM.token CST.TokEof)
    . CST.lexTopLevel

parseOne :: CST.Parser a -> CST.Parser a
parseOne p = CSTM.token CST.TokLayoutStart *> p <* CSTM.token CST.TokLayoutEnd

parseType :: Text -> Maybe D.Type'
parseType = fmap (fmap (const ()) . CST.convertType "<query>") . runParser (parseOne CST.parseTypeP)

isSymbol :: Text -> Bool
isSymbol =
  either (const False) id
    . fmap (symbol . map CST.tokValue)
    . sequence
    . CST.lex
  where
  symbol (CST.TokSymbolName _ _ : _) = True
  symbol (CST.TokOperator _ _ : _) = True
  symbol _ = False

isDeprecated :: D.Package a -> Bool
isDeprecated D.Package{..} =
  "pursuit-deprecated" `elem` bowerKeywords pkgMeta
