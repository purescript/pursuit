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
  declTypeOrValue,
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

import qualified Data.Text.Lazy as LT
import Data.Text (Text)
import qualified Data.Text as T

import Lucid hiding (for_)
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html.Renderer.Text as H
import qualified Cheapskate

import System.FilePath ((</>))

import qualified Language.PureScript as P

import Language.PureScript.Docs.Types
import Language.PureScript.Docs.RenderedCode hiding (sp)
import qualified Language.PureScript.Docs.Render as Render

import Model.DocLinks

declTypeOrValue :: Declaration -> TypeOrValue
declTypeOrValue decl = case declInfo decl of
  ValueDeclaration{}       -> Value
  AliasDeclaration{}       -> Value
  DataDeclaration{}        -> Type
  ExternDataDeclaration{}  -> Type
  TypeSynonymDeclaration{} -> Type
  TypeClassDeclaration{}   -> Type

data HtmlOutput a = HtmlOutput
  { htmlIndex     :: [(Maybe Char, a)]
  , htmlModules   :: [(P.ModuleName, HtmlOutputModule a)]
  }
  deriving (Show, Functor)

data HtmlOutputModule a = HtmlOutputModule
  { htmlOutputModuleLocals    :: a
  , htmlOutputModuleReExports :: [(P.ModuleName, a)]
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

packageAsHtml :: (P.ModuleName -> HtmlRenderContext) -> Package a -> HtmlOutput LT.Text
packageAsHtml getHtmlCtx pkg@Package{..} =
  fmap renderText (HtmlOutput indexFile modules)
  where
  linksCtx = getLinksContext pkg
  indexFile = renderIndex linksCtx
  modules = map (\m -> moduleAsHtml (getHtmlCtx (modName m)) m) pkgModules

moduleAsHtml :: HtmlRenderContext -> Module -> (P.ModuleName, HtmlOutputModule (Html ()))
moduleAsHtml r Module{..} = (modName, HtmlOutputModule html reexports)
  where
  renderDecl = declAsHtml r
  html = do
    for_ modComments renderComments
    for_ modDeclarations renderDecl
  reexports =
    map (second (foldMap renderDecl)) modReExports

renderIndex :: LinksContext -> [(Maybe Char, Html ())]
renderIndex LinksContext{..} = go ctxBookmarks
  where
  go = takeLocals
     >>> groupIndex getIndex renderEntry
     >>> map (second (ul_ . mconcat))

  getIndex (_, title) = do
    c <- textHeadMay title
    guard (toUpper c `elem` ['A'..'Z'])
    pure c

  textHeadMay t =
    case T.length t of
      0 -> Nothing
      _ -> Just (T.index t 0)

  renderEntry (mn, title) =
    li_ $ do
      let url = T.pack (filePathFor mn `relativeTo` "index") <> "#" <> title
      code_ (a_ [href_ url] (text title))
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

declAsHtml :: HtmlRenderContext -> Declaration -> Html ()
declAsHtml r d@Declaration{..} = do
  let declFragment = makeFragment (declTypeOrValue d) declTitle
  div_ [class_ "decl", id_ (T.drop 1 declFragment)] $ do
    linkTo declFragment $
      h3_ (text declTitle)
    div_ [class_ "decl-inner"] $ do
      case declInfo of
        AliasDeclaration fixity alias ->
          renderAlias fixity alias
        _ ->
          code_ [class_ "code-block"] $
            codeAsHtml r (Render.renderDeclaration d)

      for_ declComments renderComments

      let (instances, dctors, members) = partitionChildren declChildren

      unless (null dctors) $ do
        h4_ "Constructors"
        renderChildren r dctors

      unless (null members) $ do
        h4_ "Members"
        renderChildren r members

      unless (null instances) $ do
        h4_ "Instances"
        renderChildren r instances

      for_ declSourceSpan (linkToSource r)

renderChildren :: HtmlRenderContext -> [ChildDeclaration] -> Html ()
renderChildren _ [] = return ()
renderChildren r xs = ul_ $ mapM_ go xs
  where
  go decl = item decl . code_ . codeAsHtml r . Render.renderChildDeclaration $ decl
  item decl = let fragment = makeFragment (cdeclTypeOrValue decl) (cdeclTitle decl)
              in  li_ [id_ (T.drop 1 fragment)]

cdeclTypeOrValue :: ChildDeclaration -> TypeOrValue
cdeclTypeOrValue decl = case cdeclInfo decl of
  ChildInstance _ _      -> Value
  ChildDataConstructor _ -> Value
  ChildTypeClassMember _ -> Value

codeAsHtml :: HtmlRenderContext -> RenderedCode -> Html ()
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

renderLink :: HtmlRenderContext -> DocLink -> Html () -> Html ()
renderLink r link@DocLink{..} =
  a_ [ href_ (renderDocLink r link <> fragmentFor link)
     , title_ fullyQualifiedName
     ]
  where
  fullyQualifiedName = case linkLocation of
    SameModule                -> fq (currentModuleName r) linkTitle
    LocalModule _ modName     -> fq modName linkTitle
    DepsModule _ _ _ modName  -> fq modName linkTitle

  fq mn str = P.runModuleName mn <> "." <> str

-- TODO: escaping?
makeFragment :: TypeOrValue -> Text -> Text
makeFragment Type  = ("#t:" <>)
makeFragment Value = ("#v:" <>)

fragmentFor :: DocLink -> Text
fragmentFor l = makeFragment (linkTypeOrValue l) (linkTitle l)

linkToDeclaration :: HtmlRenderContext ->
                     Text ->
                     ContainingModule ->
                     Html () ->
                     Html ()
linkToDeclaration r target containMn =
  maybe id (renderLink r) (buildDocLink r target containMn)

linkToSource :: HtmlRenderContext -> P.SourceSpan -> Html ()
linkToSource r srcspan =
  p_ (linkTo (renderSourceLink r srcspan) (text "Source"))

renderAlias :: P.Fixity -> FixityAlias -> Html ()
renderAlias (P.Fixity associativity precedence) alias =
  p_ $ do
    toHtml $ "Operator alias for " <> P.showQualified showAliasName alias <> " "
    em_ (text ("(" <> associativityStr <> " / precedence " <> T.pack (show precedence) <> ")"))
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
renderComments :: Text -> Html ()
renderComments = toHtmlRaw . H.renderHtml . H.toHtml . Cheapskate.markdown opts
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

text :: Text -> Html ()
text = toHtml

sp :: Html ()
sp = text " "

withClass :: String -> Html () -> Html ()
withClass className content = span_ [class_ (fromString className)] content

linkTo :: Text -> Html () -> Html ()
linkTo href inner = a_ [href_ href] inner

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
