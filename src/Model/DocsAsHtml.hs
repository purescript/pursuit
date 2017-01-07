{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}

-- | Functions for rendering generated documentation from PureScript code as
-- HTML.

module Model.DocsAsHtml (
  HtmlOutput(..),
  HtmlOutputModule(..),
  HtmlRenderContext(..),
  nullRenderContext,
  declNamespace,
  packageAsHtml,
  moduleAsHtml,
  makeFragment
) where

import Prelude
import Control.Arrow (second)
import Control.Category ((>>>))
import Control.Monad (unless, guard)
import Data.Char (toUpper)
import Data.Ord (comparing)
import Data.Monoid ((<>))
import Data.Foldable (for_)
import Data.List (intercalate, sortBy)
import qualified Data.DList as DList
import Data.List.Split (splitOn)
import Data.Default (def)
import Data.String (fromString)
import qualified Data.Map as M

import Data.Text (Text)
import qualified Data.Text as T

import Text.Blaze.Html5 as H hiding (map)
import qualified Text.Blaze.Html5.Attributes as A
import qualified Cheapskate

import System.FilePath ((</>))

import qualified Language.PureScript as P

import Language.PureScript.Docs.Types
import Language.PureScript.Docs.RenderedCode hiding (sp)
import qualified Language.PureScript.Docs.Render as Render

import Model.DocLinks

declNamespace :: Declaration -> Namespace
declNamespace decl = case declInfo decl of
  ValueDeclaration{}       -> ValueNS
  AliasDeclaration{}       -> ValueNS
  DataDeclaration{}        -> TypeNS
  ExternDataDeclaration{}  -> TypeNS
  TypeSynonymDeclaration{} -> TypeNS
  TypeClassDeclaration{}   -> TypeNS
  ExternKindDeclaration{}  -> KindNS

data HtmlOutput a = HtmlOutput
  { htmlIndex     :: [(Maybe Char, a)]
  , htmlModules   :: [(P.ModuleName, HtmlOutputModule a)]
  }
  deriving (Show, Functor)

data HtmlOutputModule a = HtmlOutputModule
  { htmlOutputModuleLocals    :: a
  , htmlOutputModuleReExports :: [(InPackage P.ModuleName, a)]
  }
  deriving (Show, Functor)

data HtmlRenderContext = HtmlRenderContext
  { currentModuleName :: P.ModuleName
  , buildDocLink :: Text -> ContainingModule -> Maybe DocLink
  , renderDocLink :: DocLink -> Text
  , renderSourceLink :: P.SourceSpan -> Text
  }

-- |
-- An HtmlRenderContext for when you don't want to render any links.
nullRenderContext :: P.ModuleName -> HtmlRenderContext
nullRenderContext mn = HtmlRenderContext
  { currentModuleName = mn
  , buildDocLink = const (const Nothing)
  , renderDocLink = const ""
  , renderSourceLink = const ""
  }

packageAsHtml :: (P.ModuleName -> HtmlRenderContext) -> Package a -> HtmlOutput Html
packageAsHtml getHtmlCtx pkg@Package{..} =
  HtmlOutput indexFile modules
  where
  linksCtx = getLinksContext pkg
  indexFile = renderIndex linksCtx
  modules = map (\m -> moduleAsHtml (getHtmlCtx (modName m)) m) pkgModules

moduleAsHtml :: HtmlRenderContext -> Module -> (P.ModuleName, HtmlOutputModule Html)
moduleAsHtml r Module{..} = (modName, HtmlOutputModule modHtml reexports)
  where
  renderDecl = declAsHtml r
  modHtml = do
    for_ modComments renderComments
    for_ modDeclarations renderDecl
  reexports =
    map (second (foldMap renderDecl)) modReExports

renderIndex :: LinksContext -> [(Maybe Char, Html)]
renderIndex LinksContext{..} = go ctxBookmarks
  where
  go = takeLocals
     >>> groupIndex getIndex renderEntry
     >>> map (second (ul . mconcat))

  getIndex (_, title_) = do
    c <- textHeadMay title_
    guard (toUpper c `elem` ['A'..'Z'])
    pure c

  textHeadMay t =
    case T.length t of
      0 -> Nothing
      _ -> Just (T.index t 0)

  renderEntry (mn, title_) =
    li $ do
      let url = T.pack (filePathFor mn `relativeTo` "index") <> "#" <> title_
      code $
        a ! A.href (v url) $ text title_
      sp
      text ("(" <> P.runModuleName mn <> ")")

  groupIndex :: Ord i => (a -> Maybe i) -> (a -> b) -> [a] -> [(Maybe i, [b])]
  groupIndex f g =
    map (second DList.toList) . M.toList . foldr go' M.empty . sortBy (comparing f)
    where
    go' x = insertOrAppend (f x) (g x)
    insertOrAppend idx val m =
      let cur = M.findWithDefault DList.empty idx m
          new = DList.snoc cur val
      in  M.insert idx new m

declAsHtml :: HtmlRenderContext -> Declaration -> Html
declAsHtml r d@Declaration{..} = do
  let declFragment = makeFragment (declNamespace d) declTitle
  H.div ! A.class_ "decl" ! A.id (v (T.drop 1 declFragment)) $ do
    h3 ! A.class_ "decl__title clearfix" $ do
      a ! A.class_ "decl__anchor" ! A.href (v declFragment) $ "#"
      text declTitle
      for_ declSourceSpan (linkToSource r)

    H.div ! A.class_ "decl__body" $ do
      case declInfo of
        AliasDeclaration fixity alias ->
          renderAlias fixity alias
        _ ->
          pre ! A.class_ "decl__signature" $ code $
            codeAsHtml r (Render.renderDeclaration d)

      for_ declComments renderComments

      let (instances, dctors, members) = partitionChildren declChildren

      unless (null dctors) $ do
        h4 "Constructors"
        renderChildren r dctors

      unless (null members) $ do
        h4 "Members"
        renderChildren r members

      unless (null instances) $ do
        h4 "Instances"
        renderChildren r instances
  where
    linkToSource :: HtmlRenderContext -> P.SourceSpan -> Html
    linkToSource ctx srcspan =
      H.span ! A.class_ "decl__source" $
        a ! A.href (v (renderSourceLink ctx srcspan)) $ text "Source"

renderChildren :: HtmlRenderContext -> [ChildDeclaration] -> Html
renderChildren _ [] = return ()
renderChildren r xs = ul $ mapM_ go xs
  where
  go decl = item decl . code . codeAsHtml r . Render.renderChildDeclaration $ decl
  item decl = let fragment = makeFragment (cdeclNamespace decl) (cdeclTitle decl)
              in  li ! A.id (v (T.drop 1 fragment))

cdeclNamespace :: ChildDeclaration -> Namespace
cdeclNamespace decl = case cdeclInfo decl of
  ChildInstance{}        -> ValueNS
  ChildDataConstructor{} -> ValueNS
  ChildTypeClassMember{} -> ValueNS

codeAsHtml :: HtmlRenderContext -> RenderedCode -> Html
codeAsHtml r = outputWith elemAsHtml
  where
  elemAsHtml e = case e of
    Syntax x ->
      withClass "syntax" (text x)
    Ident x mn ->
      linkToDecl x mn (withClass "ident" (text x))
    Ctor x mn ->
      linkToDecl x mn (withClass "ctor" (text x))
    Kind x ->
      text x
    Keyword x ->
      withClass "keyword" (text x)
    Space ->
      text " "

  linkToDecl = linkToDeclaration r

renderLink :: HtmlRenderContext -> DocLink -> Html -> Html
renderLink r link_@DocLink{..} =
  a ! A.href (v (renderDocLink r link_ <> fragmentFor link_))
    ! A.title (v fullyQualifiedName)
  where
  fullyQualifiedName = case linkLocation of
    SameModule                -> fq (currentModuleName r) linkTitle
    LocalModule _ modName     -> fq modName linkTitle
    DepsModule _ _ _ modName  -> fq modName linkTitle
    BuiltinModule modName     -> fq modName linkTitle

  fq mn str = P.runModuleName mn <> "." <> str

makeFragment :: Namespace -> Text -> Text
makeFragment ns = (prefix <>) . escape
  where
  prefix = case ns of
    TypeNS -> "#t:"
    ValueNS -> "#v:"
    KindNS -> "#k:"

  -- TODO
  escape = id

fragmentFor :: DocLink -> Text
fragmentFor l = makeFragment (linkNamespace l) (linkTitle l)

linkToDeclaration :: HtmlRenderContext ->
                     Text ->
                     ContainingModule ->
                     Html ->
                     Html
linkToDeclaration r target containMn =
  maybe id (renderLink r) (buildDocLink r target containMn)

renderAlias :: P.Fixity -> FixityAlias -> Html
renderAlias (P.Fixity associativity precedence) alias =
  p $ do
    toHtml $ "Operator alias for " <> P.showQualified showAliasName alias <> " "
    em $
      text ("(" <> associativityStr <> " / precedence " <> T.pack (show precedence) <> ")")
  where
  showAliasName (Left valueAlias) = P.runProperName valueAlias
  showAliasName (Right typeAlias) = case typeAlias of
    (Left identifier)  -> P.runIdent identifier
    (Right properName) -> P.runProperName properName
  associativityStr = case associativity of
    P.Infixl -> "left-associative"
    P.Infixr -> "right-associative"
    P.Infix  -> "non-associative"

-- TODO: use GitHub API instead?
renderComments :: Text -> Html
renderComments = H.toHtml . Cheapskate.markdown opts
  where
  opts = def { Cheapskate.allowRawHtml = False }

-- | if `to` and `from` are both files in the current package, generate a
-- FilePath for `to` relative to `from`.
--
-- TODO: Remove this
relativeTo :: FilePath -> FilePath -> FilePath
relativeTo to from = go (splitOn "/" to) (splitOn "/" from)
  where
  go (x : xs) (y : ys) | x == y = go xs ys
  go xs ys = intercalate "/" $ replicate (length ys - 1) ".." ++ xs

-- | Generate a FilePath for module documentation for a module in the current
-- package.
--
-- TODO: Remove this
filePathFor :: P.ModuleName -> FilePath
filePathFor (P.ModuleName parts) = go parts
  where
  go [] = "index.html"
  go (x : xs) = show x </> go xs

sp :: Html
sp = text " "

v :: Text -> AttributeValue
v = toValue

withClass :: String -> Html -> Html
withClass className content = H.span ! A.class_ (fromString className) $ content

partitionChildren ::
  [ChildDeclaration] ->
  ([ChildDeclaration], [ChildDeclaration], [ChildDeclaration])
partitionChildren = foldl go ([], [], [])
  where
  go (instances, dctors, members) rcd =
    case cdeclInfo rcd of
      ChildInstance _ _      -> (rcd : instances, dctors, members)
      ChildDataConstructor _ -> (instances, rcd : dctors, members)
      ChildTypeClassMember _ -> (instances, dctors, rcd : members)
