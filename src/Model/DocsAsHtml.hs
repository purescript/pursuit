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
import Data.Version
import qualified Data.Map as M

import qualified Data.Text.Lazy as LT
import qualified Data.Text as T

import Lucid hiding (for_)
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html.Renderer.Text as H
import qualified Cheapskate

import System.FilePath ((</>))

import Web.Bower.PackageMeta hiding (Version)

import qualified Language.PureScript as P

import Language.PureScript.Docs.Types
import Language.PureScript.Docs.RenderedCode hiding (sp)

import Model.DocLinks

data HtmlOutput a = HtmlOutput
  { htmlIndex     :: [(Maybe Char, a)]
  , htmlModules   :: [(P.ModuleName, a)]
  }
  deriving (Show)

packageAsHtml :: Package a -> HtmlOutput LT.Text
packageAsHtml pkg@Package{..} =
  HtmlOutput (htmlAsText indexFile) (htmlAsText modules)
  where
  ctx = getLinksContext pkg
  indexFile = renderIndex ctx
  modules = map (moduleAsHtml ctx) pkgModules
  htmlAsText = map (second renderText)

moduleAsHtml :: LinksContext -> RenderedModule -> (P.ModuleName, Html ())
moduleAsHtml ctx RenderedModule{..} = (mn, html)
  where
  mn = P.moduleNameFromString rmName
  ctx' = (ctx, mn)
  html = do
    for_ rmComments renderComments
    for_ rmDeclarations (declAsHtml ctx')

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

declAsHtml :: LinksContext' -> RenderedDeclaration -> Html ()
declAsHtml ctx RenderedDeclaration{..} =
  div_ [class_ "decl", id_ ("d:" <> T.pack rdTitle)] $ do
    a_ [href_ (T.pack (fragmentFor rdTitle))] $
      h3_ (text rdTitle)
    div_ [class_ "decl-inner"] $ do
      code_ [class_ "code-block"] $
        codeAsHtml ctx rdCode

      for_ rdFixity renderFixity
      for_ rdComments renderComments

      let (instances, dctors, members) = partitionChildren rdChildren

      when (not (null dctors)) $ do
        h4_ "Constructors"
        renderChildren ctx dctors

      when (not (null members)) $ do
        h4_ "Members"
        renderChildren ctx members

      when (not (null instances)) $ do
        h4_ "Instances"
        renderChildren ctx instances

      for_ rdSourceSpan (linkToSource ctx)

renderChildren :: LinksContext' -> [RenderedChildDeclaration] -> Html ()
renderChildren _   [] = return ()
renderChildren ctx xs = go xs
  where
  go = ul_ . mapM_ (li_ . code_ . codeAsHtml ctx . code . rcdInfo)
  code (ChildInstance c) = c
  code (ChildDataConstructor c _) = c
  code (ChildTypeClassMember c _) = c

codeAsHtml :: LinksContext' -> RenderedCode -> Html ()
codeAsHtml ctx = outputWith elemAsHtml
  where
  elemAsHtml (Syntax x)  = withClass "syntax" (text x)
  elemAsHtml (Ident x)   = withClass "ident" (text x)
  elemAsHtml (Ctor x mn) = linkToConstructor ctx x mn (withClass "ctor" (text x))
  elemAsHtml (Kind x)    = text x
  elemAsHtml (Keyword x) = withClass "keyword" (text x)
  elemAsHtml Space       = text " "

renderLink :: DocLink -> Html () -> Html ()
renderLink (SameModule x) = linkTo (fragmentFor x)
renderLink (LocalModule srcMn destMn x) =
  let uri = filePathFor destMn `relativeTo` filePathFor srcMn
  in  linkTo (uri ++ fragmentFor x)
renderLink (DepsModule srcMn pkgName pkgVersion destMn x) =
  let relativeTo' = relativeToOtherPackage pkgName pkgVersion
      uri = filePathFor destMn `relativeTo'` filePathFor srcMn
  in  linkTo (uri ++ fragmentFor x)

-- TODO: escaping?
fragmentFor :: String -> String
fragmentFor = ("#d:" ++)

linkToConstructor :: LinksContext' -> String -> ContainingModule -> Html () -> Html ()
linkToConstructor ctx ctor' containMn =
  maybe id renderLink (getLink ctx ctor' containMn)

linkToSource :: LinksContext' -> P.SourceSpan -> Html ()
linkToSource (LinksContext{..}, _) (P.SourceSpan name start end) =
  p_ (linkTo (concat
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
relativeTo :: FilePath -> FilePath -> FilePath
relativeTo to from = go (splitOn "/" to) (splitOn "/" from)
  where
  go (x : xs) (y : ys) | x == y = go xs ys
  go xs ys = intercalate "/" $ replicate (length ys - 1) ".." ++ xs

-- | Generate a FilePath for module documentation for a module in the current
-- package.
filePathFor :: P.ModuleName -> FilePath
filePathFor (P.ModuleName parts) = go parts
  where
  go [] = "index.html"
  go (x : xs) = show x </> go xs

-- | Like `relativeTo`, but in the case where `to` is in another package.
relativeToOtherPackage :: PackageName -> Version -> FilePath -> FilePath -> FilePath
relativeToOtherPackage name (showVersion -> vers) to from =
  intercalate "/" (dots ++ [runPackageName name, vers] ++ splitOn "/" to)
  where
  dots = replicate (length from - 1) ".."

text :: String -> Html ()
text = toHtml

sp :: Html ()
sp = text " "

withClass :: String -> Html () -> Html ()
withClass className content = span_ [class_ (fromString className)] content

linkTo :: String -> Html () -> Html ()
linkTo href inner = a_ [href_ (fromString href)] inner

partitionChildren ::
  [RenderedChildDeclaration] ->
  ([RenderedChildDeclaration], [RenderedChildDeclaration], [RenderedChildDeclaration])
partitionChildren = foldl go ([], [], [])
  where
  go (instances, dctors, members) rcd =
    case rcdInfo rcd of
      ChildInstance _          -> (rcd : instances, dctors, members)
      ChildDataConstructor _ _ -> (instances, rcd : dctors, members)
      ChildTypeClassMember _ _ -> (instances, dctors, rcd : members)
