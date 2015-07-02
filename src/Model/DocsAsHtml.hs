{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- | Functions for rendering generated documentation from PureScript code as
-- HTML.

module Model.DocsAsHtml (
  HtmlOutput(..),
  packageAsHtml
) where

import Prelude
import Control.Arrow (second)
import Control.Category ((>>>))
import Control.Monad (when)
import Data.Char (toUpper)
import Data.Ord (comparing)
import Data.Monoid (mconcat, (<>))
import Data.Foldable (for_)
import Data.List (intercalate, sortBy)
import qualified Data.DList as DList
import Data.List.Split (splitOn)
import Data.Default (def)
import Data.String (fromString)
import qualified Data.Map as M

import qualified Data.Text.Lazy as LT
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

data HtmlOutput a = HtmlOutput
  { htmlIndex     :: [(Maybe Char, a)]
  , htmlModules   :: [(P.ModuleName, a)]
  }
  deriving (Show)

type DocLinkRenderer = LinksContext' -> DocLink -> T.Text

packageAsHtml :: DocLinkRenderer -> Package a -> HtmlOutput LT.Text
packageAsHtml r pkg@Package{..} =
  HtmlOutput (htmlAsText indexFile) (htmlAsText modules)
  where
  ctx = getLinksContext pkg
  indexFile = renderIndex ctx
  modules = map (moduleAsHtml r ctx) pkgModules
  htmlAsText = map (second renderText)

moduleAsHtml :: DocLinkRenderer -> LinksContext -> Module -> (P.ModuleName, Html ())
moduleAsHtml r ctx Module{..} = (mn, html)
  where
  mn = P.moduleNameFromString modName
  ctx' = (ctx, mn)
  html = do
    for_ modComments renderComments
    for_ modDeclarations (declAsHtml r ctx')

renderIndex :: LinksContext -> [(Maybe Char, Html ())]
renderIndex LinksContext{..} = go ctxBookmarks
  where
  go = takeLocals
     >>> groupIndex getIndex renderEntry
     >>> map (second (ul_ . mconcat))

  getIndex (_, ((toUpper -> c) :_))
    | c `elem` ['A'..'Z'] = Just c
    | otherwise = Nothing
  getIndex _ = Nothing

  renderEntry (mn, title) =
    li_ $ do
      let url = T.pack ((filePathFor mn `relativeTo` "index") ++ "#" ++ title)
      code_ (a_ [href_ url] (text title))
      sp
      text ("(" ++ show mn ++ ")")

  groupIndex :: Ord i => (a -> Maybe i) -> (a -> b) -> [a] -> [(Maybe i, [b])]
  groupIndex f g =
    map (second DList.toList) . M.toList . foldr go' M.empty . sortBy (comparing f)
    where
    go' x = insertOrAppend (f x) (g x)
    insertOrAppend idx val m =
      let cur = M.findWithDefault DList.empty idx m
          new = DList.snoc cur val
      in  M.insert idx new m

declAsHtml :: DocLinkRenderer -> LinksContext' -> Declaration -> Html ()
declAsHtml r ctx d@Declaration{..} = do
  let declFragment = T.pack $ makeFragment declTitle
  div_ [class_ "decl", id_ (T.drop 1 declFragment)] $ do
    linkTo declFragment $
      h3_ (text declTitle)
    div_ [class_ "decl-inner"] $ do
      code_ [class_ "code-block"] $
        codeAsHtml r ctx (Render.renderDeclaration d)

      for_ declFixity renderFixity
      for_ declComments renderComments

      let (instances, dctors, members) = partitionChildren declChildren

      when (not (null dctors)) $ do
        h4_ "Constructors"
        renderChildren r ctx dctors

      when (not (null members)) $ do
        h4_ "Members"
        renderChildren r ctx members

      when (not (null instances)) $ do
        h4_ "Instances"
        renderChildren r ctx instances

      for_ declSourceSpan (linkToSource ctx)

renderChildren :: DocLinkRenderer -> LinksContext' -> [ChildDeclaration] -> Html ()
renderChildren _ _   [] = return ()
renderChildren r ctx xs = go xs
  where
  go = ul_ . mapM_ (li_ . code_ . codeAsHtml r ctx . Render.renderChildDeclaration)

codeAsHtml :: DocLinkRenderer -> LinksContext' -> RenderedCode -> Html ()
codeAsHtml r ctx = outputWith elemAsHtml
  where
  elemAsHtml (Syntax x)  = withClass "syntax" (text x)
  elemAsHtml (Ident x)   = withClass "ident" (text x)
  elemAsHtml (Ctor x mn) = linkToConstructor r ctx x mn (withClass "ctor" (text x))
  elemAsHtml (Kind x)    = text x
  elemAsHtml (Keyword x) = withClass "keyword" (text x)
  elemAsHtml Space       = text " "

renderLink :: DocLinkRenderer -> LinksContext' -> DocLink -> Html () -> Html ()
renderLink r ctx link = linkTo (r ctx link <> T.pack (fragmentFor link))

-- TODO: escaping?
makeFragment :: String -> String
makeFragment = ("#d:" ++)

fragmentFor :: DocLink -> String
fragmentFor = makeFragment . title
  where
  title (SameModule t) = t
  title (LocalModule _ _ t) = t
  title (DepsModule _ _ _ _ t) = t

linkToConstructor :: DocLinkRenderer -> LinksContext' -> String -> ContainingModule -> Html () -> Html ()
linkToConstructor r ctx ctor' containMn =
  maybe id (renderLink r ctx) (getLink ctx ctor' containMn)

linkToSource :: LinksContext' -> P.SourceSpan -> Html ()
linkToSource (LinksContext{..}, _) (P.SourceSpan name start end) =
  p_ (linkTo (T.pack $ concat
               [ githubBaseUrl
               , "/tree/master/"
               , relativeToBase name
               , "#", fragment
               ])
             (text "Source"))
  where
  (P.SourcePos startLine _) = start
  (P.SourcePos endLine _) = end
  (GithubUser user, GithubRepo repo) = ctxGithub

  relativeToBase = intercalate "/" . dropWhile (/= "src") . splitOn "/"
  githubBaseUrl = concat ["https://github.com/", user, "/", repo]
  fragment = "L" ++ show startLine ++ "-L" ++ show endLine

renderFixity :: P.Fixity -> Html ()
renderFixity (P.Fixity associativity precedence) =
  p_ (em_ (text (associativityStr <> " / precedence " <> show precedence)))
  where
  associativityStr = case associativity of
    P.Infixl -> "left-associative"
    P.Infixr -> "right-associative"
    P.Infix  -> "non-associative"

-- TODO: use GitHub API instead?
renderComments :: String -> Html ()
renderComments = toHtmlRaw . H.renderHtml . H.toHtml . Cheapskate.markdown def . T.pack

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

text :: String -> Html ()
text = toHtml

sp :: Html ()
sp = text " "

withClass :: String -> Html () -> Html ()
withClass className content = span_ [class_ (fromString className)] content

linkTo :: T.Text -> Html () -> Html ()
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
