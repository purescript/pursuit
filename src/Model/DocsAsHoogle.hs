{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.DocsAsHoogle
  ( packageAsHoogle
  ) where

-- | Functions for rendering generated documentation from PureScript code as
-- | an input file, suitable for supplying to Hoogle to build a database from.

import Prelude
import Data.Foldable (foldMap)
import Data.Monoid
import Data.Version (showVersion)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html.Renderer.Text as H
import qualified Cheapskate

import qualified Language.PureScript.Docs as D
import qualified Web.Bower.PackageMeta as Bower

packageAsHoogle :: D.Package a -> LT.Text
packageAsHoogle pkg@D.Package{..} = preamble <> modules
  where
  preamble =
    LT.unlines [ "@package " <> (LT.pack $ Bower.runPackageName $ D.packageName pkg)
               , "@version " <> (LT.pack $ showVersion $ pkgVersion)
               , ""
               ]
  modules =
    foldMap moduleAsHoogle pkgModules

codeAsHoogle :: D.RenderedCode -> LT.Text
codeAsHoogle = D.outputWith elemAsText
  where
  elemAsText (D.Syntax x)  = LT.pack x
  elemAsText (D.Ident x)   = LT.pack x
  elemAsText (D.Ctor x _)  = LT.pack x
  elemAsText (D.Kind x)    = LT.pack x
  elemAsText (D.Keyword x) = LT.pack x
  elemAsText D.Space       = " "

declAsHoogle :: D.RenderedDeclaration -> LT.Text
declAsHoogle D.RenderedDeclaration{..} =
     commentsAsHoogle rdComments
  <> codeAsHoogle rdCode
  <> "\n\n"
  <> foldMap ((<> "\n\n") . childDeclAsHoogle) rdChildren

childDeclAsHoogle :: D.RenderedChildDeclaration -> LT.Text
childDeclAsHoogle D.RenderedChildDeclaration{..} =
  commentsAsHoogle rcdComments <> codeAsHoogle code
  where
  code = case rcdInfo of
    D.ChildInstance c -> c
    D.ChildDataConstructor _ c -> c
    D.ChildTypeClassMember _ c -> c

moduleAsHoogle :: D.RenderedModule -> LT.Text
moduleAsHoogle D.RenderedModule{..} =
  commentsAsHoogle rmComments
    <> "module " <> LT.pack rmName <> " where\n\n"
    <> foldMap ((<> "\n\n") . declAsHoogle) rmDeclarations

commentsAsHoogle :: Maybe String -> LT.Text
commentsAsHoogle =
  maybe "" (LT.unlines . fmap ("-- | " <>) . LT.lines . renderComments)

renderComments :: String -> LT.Text
renderComments =
  H.renderHtml . H.toHtml . renderMarkdown . T.pack
  where
  renderMarkdown = Cheapskate.markdown Cheapskate.def
