{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Model.DocsAsHoogle
  ( packageAsHoogle
  ) where

-- | Functions for rendering generated documentation from PureScript code as
-- | an input file, suitable for supplying to Hoogle to build a database from.

import Prelude
import Control.Arrow (first, second)
import Data.Foldable (foldMap)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Version (showVersion)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html.Renderer.Text as H
import qualified Cheapskate

import qualified Language.PureScript as P
import qualified Language.PureScript.Docs as D
import qualified Web.Bower.PackageMeta as Bower

import Model.DocLinks

packageAsHoogle :: D.Package a -> LT.Text
packageAsHoogle pkg@D.Package{..} = preamble <> modules
  where
  preamble =
    LT.unlines [ "@package " <> (LT.pack $ Bower.runPackageName $ D.packageName pkg)
               , "@version " <> (LT.pack $ showVersion $ pkgVersion)
               , ""
               ]
  ctx = getLinksContext pkg
  modules = foldMap renderModule pkgModules
  renderModule m = moduleAsHoogle (ctx, P.moduleNameFromString (D.modName m)) m

codeAsHoogle :: LinksContext' -> D.RenderedCode -> LT.Text
codeAsHoogle ctx = D.outputWith elemAsText
  where
  elemAsText (D.Syntax x)  = LT.pack x
  elemAsText (D.Ident x)   = LT.pack x
  elemAsText (D.Ctor x mn) = qualifyConstructor ctx x mn
  elemAsText (D.Kind x)    = LT.pack x
  elemAsText (D.Keyword x) = LT.pack x
  elemAsText D.Space       = " "

qualifyConstructor :: LinksContext' -> String -> D.ContainingModule -> LT.Text
qualifyConstructor ctx ctor' containMn =
  maybe (LT.pack ctor') render (getLink ctx ctor' containMn)
  where
  render docLink = LT.pack $ case docLink of
    SameModule ctor ->
      show (snd ctx) ++ "." ++ ctor
    LocalModule _ otherMn ctor ->
      show otherMn ++ "." ++ ctor
    DepsModule _ otherPkg _ otherMn ctor ->
      Bower.runPackageName otherPkg ++ ":" ++ show otherMn ++ "." ++ ctor

declAsHoogle :: LinksContext' -> D.Declaration -> LT.Text
declAsHoogle ctx d@D.Declaration{..} =
     commentsAsHoogle declComments
  <> codeAsHoogle ctx (renderDeclaration d)
  <> "\n\n"
  <> foldMap ((<> "\n\n") . childDeclAsHoogle ctx d) declChildren

childDeclAsHoogle :: LinksContext' -> D.Declaration -> D.ChildDeclaration -> LT.Text
childDeclAsHoogle ctx parent d@D.ChildDeclaration{..} =
     commentsAsHoogle cdeclComments
  <> codeAsHoogle ctx (renderChildDeclaration parent d)

moduleAsHoogle :: LinksContext' -> D.Module -> LT.Text
moduleAsHoogle ctx D.Module{..} =
  commentsAsHoogle modComments
    <> "module " <> LT.pack modName <> " where\n\n"
    <> foldMap ((<> "\n\n") . declAsHoogle ctx) modDeclarations

commentsAsHoogle :: Maybe String -> LT.Text
commentsAsHoogle =
  maybe "" (LT.unlines . fmap ("-- | " <>) . LT.lines . renderComments)

renderComments :: String -> LT.Text
renderComments =
  H.renderHtml . H.toHtml . renderMarkdown . T.pack
  where
  renderMarkdown = Cheapskate.markdown Cheapskate.def

renderDeclaration :: D.Declaration -> D.RenderedCode
renderDeclaration =
  D.renderDeclarationWithOptions hoogleRenderTypeOptions . preprocessDeclaration

-- | Renders code in a child declaration, with the following differences from
-- the normal output:
--
-- * Data constructors are presented like value declarations, eg
--   `Just :: forall a. a -> Maybe a`.
--
-- * Type class members include a constraint for the relevant type class.
--
-- * Type instances' names are omitted (in order to be parseable as Haskell
--   code).
renderChildDeclaration :: D.Declaration -> D.ChildDeclaration -> D.RenderedCode
renderChildDeclaration
  (preprocessDeclaration -> parent)
  (preprocessChildDeclaration -> D.ChildDeclaration{..}) =
    case cdeclInfo of
      D.ChildDataConstructor tys ->
        D.ident cdeclTitle <> D.sp <> D.syntax "::" <> D.sp <> renderType ty
        where
        ty = P.quantify $ foldr (\a b -> P.TypeApp (P.TypeApp P.tyFunction a) b) parentType tys
        parentType =
          case D.declInfo parent of
            D.DataDeclaration _ args ->
              D.typeApp (D.declTitle parent) args
            _ ->
              invalidArgument $
                "the parent of a data constructor was something other than a "
                <> "data declaration"
      D.ChildTypeClassMember ty ->
        D.ident cdeclTitle <> D.sp <>
          D.syntax "::" <> D.sp <>
          renderType (addConstraint classConstraint ty)
        where
        classConstraint =
          case D.declInfo parent of
            D.TypeClassDeclaration args _ ->
              (P.Qualified Nothing (P.ProperName (D.declTitle parent)), map D.toTypeVar args)
            _ ->
              invalidArgument $
                "the parent of a type class member was something other than a "
                <> "type class"

        addConstraint c ty' = P.moveQuantifiersToFront (P.quantify (P.ConstrainedType [c] ty'))
      D.ChildInstance constraints ty ->
        D.keywordInstance <> D.sp <>
          fromMaybe mempty (renderConstraints constraints) <> D.sp <>
          renderType ty

  where
  invalidArgument msg =
    error $ "Invalid argument in Model.DocsAsHoogle.renderChildDeclaration: " <> msg

hoogleRenderTypeOptions :: D.RenderTypeOptions
hoogleRenderTypeOptions =
  D.defaultRenderTypeOptions { D.prettyPrintObjects = False }

renderType :: P.Type -> D.RenderedCode
renderType = D.renderTypeWithOptions hoogleRenderTypeOptions

renderConstraints :: [P.Constraint] -> Maybe D.RenderedCode
renderConstraints = D.renderConstraintsWithOptions hoogleRenderTypeOptions

preprocessDeclaration :: D.Declaration -> D.Declaration
preprocessDeclaration decl =
  decl { D.declInfo = everywhereOnTypesInfo transformRows (D.declInfo decl) }

preprocessChildDeclaration :: D.ChildDeclaration -> D.ChildDeclaration
preprocessChildDeclaration decl =
  decl { D.cdeclInfo = everywhereOnTypesChildInfo transformRows (D.cdeclInfo decl) }

-- | Transform rows into a different encoding which can be understood by a
-- Haskell parser, as this makes things simpler on the Hoogle end.
transformRows :: P.Type -> P.Type
transformRows ty' =
  case ty' of
    P.RCons{} -> uncurry joinTail $ first transformHead $ P.rowToList ty'
    P.REmpty -> emptyRow
    other -> other
  where
  transformHead = foldl P.TypeApp rowCtor . map (\(l, ty) -> P.TypeApp (labelCtor l) ty)
  joinTail hd tl = P.TypeApp hd (P.TypeApp rowTailCtor tl)

  rowCtor = mkCtor "PS_Row"
  emptyRow = mkCtor "PS_Empty_Row"
  rowTailCtor = mkCtor "PS_Row_Tail"
  labelCtor label = mkCtor $ "PS_Label_" ++ label

  mkCtor = P.TypeConstructor . P.Qualified Nothing . P.ProperName

-- |
-- Transform all Language.PureScript.Type values within a given DeclarationInfo
-- by applying the provided function to each one. This uses
-- Language.PureScript.everywhereOnTypesTopDown internally so that types nested
-- within other types are also transformed.
--
everywhereOnTypesInfo :: (P.Type -> P.Type) -> D.DeclarationInfo -> D.DeclarationInfo
everywhereOnTypesInfo f info =
  case info of
    D.ValueDeclaration ty -> D.ValueDeclaration (go ty)
    D.TypeSynonymDeclaration args ty -> D.TypeSynonymDeclaration args (go ty)
    D.TypeClassDeclaration args implies -> D.TypeClassDeclaration args (map (second (map go)) implies)
    other -> other
  where
  go = P.everywhereOnTypesTopDown f

-- |
-- Much like everywhereOnTypesInfo, except that this works on child
-- declarations.
--
everywhereOnTypesChildInfo :: (P.Type -> P.Type) -> D.ChildDeclarationInfo -> D.ChildDeclarationInfo
everywhereOnTypesChildInfo f info =
  case info of
    D.ChildDataConstructor tys -> D.ChildDataConstructor (map go tys)
    D.ChildInstance constraints ty -> D.ChildInstance (map (second (map go)) constraints) (go ty)
    D.ChildTypeClassMember ty -> D.ChildTypeClassMember (go ty)
  where
  go = P.everywhereOnTypesTopDown f