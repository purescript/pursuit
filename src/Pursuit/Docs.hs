{-# LANGUAGE OverloadedStrings, TemplateHaskell, TupleSections #-}

module Pursuit.Docs where

import Control.Applicative
import Control.Monad

import Data.Default (def)
import Data.String (fromString)
import Data.Foldable (for_)

import qualified Data.Text.Lazy as TL

import qualified Language.PureScript as P

import Text.Blaze.Html ((!))
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html.Renderer.Text as H
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Cheapskate

import Pursuit.Data

declarationDocs :: Maybe [P.DeclarationRef] -> P.Declaration -> TL.Text
declarationDocs exps decl = H.renderHtml (renderDeclaration exps decl)
  where
  renderDeclaration :: Maybe [P.DeclarationRef] -> P.Declaration -> H.Html
  renderDeclaration _ (P.TypeDeclaration ident ty) =
    para "decl" $ H.code $ do
      withClass "ident" . text . show $ ident
      sp *> withClass "syntax" (text "::") <* sp
      typeToHtml ty
  renderDeclaration _ (P.ExternDeclaration _ ident _ ty) =
    para "decl" $ H.code $ do
      withClass "ident" . text . show $ ident
      sp *> withClass "syntax" (text "::") <* sp
      typeToHtml ty
  renderDeclaration exps (P.DataDeclaration dtype name args ctors) = do
    let typeApp  = foldl P.TypeApp (P.TypeConstructor (P.Qualified Nothing name)) (map toTypeVar args)
        exported = filter (P.isDctorExported name exps . fst) ctors
    para "decl" $ H.code $ do
      withClass "keyword" . text $ show dtype
      sp
      typeToHtml typeApp
    unless (null exported) $ do
      H.ul $ for_ exported $ \(ctor, tys) -> H.li . H.code $ do
        let typeApp = foldl P.TypeApp (P.TypeConstructor (P.Qualified Nothing ctor)) tys
        typeToHtml typeApp
  renderDeclaration _ (P.ExternDataDeclaration name kind) = do
    para "decl" $ H.code $ do
      withClass "keyword" . text $ "data"
      sp
      typeToHtml $ P.TypeConstructor (P.Qualified Nothing name)
      sp *> withClass "syntax" (text "::") <* sp
      text $ P.prettyPrintKind kind
  renderDeclaration _ (P.TypeSynonymDeclaration name args ty) = do
    let typeApp  = foldl P.TypeApp (P.TypeConstructor (P.Qualified Nothing name)) (map toTypeVar args)
    para "decl" $ H.code $ do
      withClass "keyword" . text $ "type"
      sp
      typeToHtml typeApp
      sp *> withClass "syntax" (text "=") <* sp
      typeToHtml ty
  renderDeclaration _ (P.TypeClassDeclaration name args implies ds) = do
    para "decl" $ H.code $ do
      withClass "keyword" (text "class") <* sp
      case implies of
        [] -> ""
        _ -> do withClass "syntax" $ text "("
                intercalateA_ (withClass "syntax" "," <* sp) $ flip map implies $ \(pn, tys') -> do
                  let supApp = foldl P.TypeApp (P.TypeConstructor pn) tys'
                  typeToHtml supApp
                withClass "syntax" $ text ") <= "
      let classApp  = foldl P.TypeApp (P.TypeConstructor (P.Qualified Nothing name)) (map toTypeVar args)
      typeToHtml classApp
    unless (null ds) $ do
      H.div ! A.class_ "typeclass" $ mapM_ renderClassMember ds
    where
    renderClassMember :: P.Declaration -> H.Html
    renderClassMember (P.PositionedDeclaration _ _ d) = renderClassMember d
    renderClassMember (P.TypeDeclaration ident ty) =
      para "decl" $ H.code $ do
        withClass "ident" . text . show $ ident
        sp *> withClass "syntax" (text "::") <* sp
        typeToHtml ty
    renderClassMember _ = error "Invalid argument to renderClassMember."
  renderDeclaration _ (P.TypeInstanceDeclaration name constraints className tys _) = do
    para "decl" $ H.code $ do
      withClass "keyword" (text "instance") <* sp
      withClass "ident" (text (show name)) <* sp
      withClass "syntax" (text "::") <* sp
      case constraints of
        [] -> ""
        _ -> do withClass "syntax" $ text "("
                intercalateA_ (withClass "syntax" "," <* sp) $ flip map constraints $ \(pn, tys') -> do
                  let supApp = foldl P.TypeApp (P.TypeConstructor pn) tys'
                  typeToHtml supApp
                withClass "syntax" $ text ") => "
      let classApp = foldl P.TypeApp (P.TypeConstructor className) tys
      typeToHtml classApp
  renderDeclaration exps (P.PositionedDeclaration _ com d) = do
    renderDeclaration exps d
    renderComments com
  renderDeclaration _ _ = return ()

  renderComments :: [P.Comment] -> H.Html
  renderComments cs = do
    let raw = concatMap toLines cs

    if all hasPipe raw
      then H.toHtml . Cheapskate.markdown def . fromString . unlines . map stripPipes $ raw
      else H.pre . H.code . H.toHtml $ unlines raw
    where

    toLines (P.LineComment s) = [s]
    toLines (P.BlockComment s) = lines s

    hasPipe s = case dropWhile (== ' ') s of { ('|':_) -> True; _ -> False }

    stripPipes = dropPipe . dropWhile (== ' ')

    dropPipe ('|':' ':s) = s
    dropPipe ('|':s) = s
    dropPipe s = s

  toTypeVar :: (String, Maybe P.Kind) -> P.Type
  toTypeVar (s, Nothing) = P.TypeVar s
  toTypeVar (s, Just k) = P.KindedType (P.TypeVar s) k

typeToHtml :: P.Type -> H.Html
typeToHtml = go 0 . P.everywhereOnTypes dePrim . P.everywhereOnTypesTopDown convertForAlls . P.everywhereOnTypes convert
  where
  go :: Int -> P.Type -> H.Html
  go _ P.TypeWildcard = withClass "syntax" (text "_")
  go _ (P.TypeVar var) = withClass "ident" (text var)
  go _ (P.PrettyPrintObject row) = do
    withClass "syntax" (text "{") <* sp
    rowToHtml row
    sp *> withClass "syntax" (text "}")
  go _ (P.PrettyPrintArray ty) = do
    withClass "syntax" (text "[")
    go 0 ty
    withClass "syntax" (text "]")
  go _ (P.TypeConstructor ctor@(P.Qualified _ name)) = H.a ! A.href (fromString ("/?q=" ++ show name)) $ withClass "ctor" (text (show name))
  go n (P.ConstrainedType deps ty) = do
    withClass "syntax" (text "(")
    intercalateA_ (withClass "syntax" (text ",") <* sp) $ flip map deps $ \(pn, tys) -> do
      let instApp = foldl P.TypeApp (P.TypeConstructor pn) tys
      go 0 instApp
    withClass "syntax" (text ")") *> sp
    withClass "syntax" (text "=>") *> sp
    go n ty
  go _ P.REmpty = withClass "syntax" (text "()")
  go _ row@P.RCons{} = do
    withClass "syntax" (text "(")
    rowToHtml row
    withClass "syntax" (text ")")
  go n (P.PrettyPrintFunction arg ret) | n < 1 = do
    go (n + 1) arg *> sp
    withClass "syntax" (text "->") *> sp
    go 0 ret
  go n (P.PrettyPrintForAll idents ty) | n < 1 = do
    withClass "keyword" (text "forall") *> sp
    intercalateA_ sp $ flip map idents $ withClass "ident" . text
    withClass "syntax" (text ".") *> sp
    go 0 ty
  go n (P.TypeApp ty1 ty2) | n < 1 = do
    go n ty1
    sp
    go (n + 1) ty2
  go _ ty = do
    withClass "syntax" (text "(")
    go 0 ty
    withClass "syntax" (text ")")

  dePrim ty@(P.TypeConstructor (P.Qualified _ name))
    | ty == P.tyBoolean || ty == P.tyNumber || ty == P.tyString =
      P.TypeConstructor $ P.Qualified Nothing name
  dePrim other = other

  convert (P.TypeApp (P.TypeApp f arg) ret) | f == P.tyFunction = P.PrettyPrintFunction arg ret
  convert (P.TypeApp a el) | a == P.tyArray = P.PrettyPrintArray el
  convert (P.TypeApp o r) | o == P.tyObject = P.PrettyPrintObject r
  convert other = other

  convertForAlls (P.ForAll ident ty _) = go [ident] ty
    where
    go idents (P.ForAll ident' ty' _) = go (ident' : idents) ty'
    go idents other = P.PrettyPrintForAll idents other
  convertForAlls other = other

rowToHtml :: P.Type -> H.Html
rowToHtml = uncurry rowToHtml' . P.rowToList
  where
  rowToHtml' h t = do
    headToHtml h
    tailToHtml t

  headToHtml = intercalateA_ (withClass "syntax" (text ",") <* sp) . map labelToHtml

  labelToHtml (label, ty) = do
    withClass "ident" (text label) <* sp
    withClass "syntax" (text "::") <* sp
    typeToHtml ty

  tailToHtml P.REmpty = return ()
  tailToHtml other = do
    sp *> withClass "syntax" (text "|") <* sp
    typeToHtml other

text :: String -> H.Html
text = H.toHtml

sp :: H.Html
sp = text " "

withClass :: H.AttributeValue -> H.Html -> H.Html
withClass className content = H.span ! A.class_ className $ content

para :: H.AttributeValue -> H.Html -> H.Html
para className content = H.p ! A.class_ className $ content

intercalateA_ :: (Applicative m) => m b -> [m a] -> m ()
intercalateA_ _   []     = pure ()
intercalateA_ _   [x]    = void x
intercalateA_ sep (x:xs) = (x <* sep) *> intercalateA_ sep xs

dataConstructorDocs :: Maybe [P.DeclarationRef] -> (P.ProperName, P.ProperName, [P.Type]) -> Maybe TL.Text
dataConstructorDocs exps (tyName, ctorName, args) = do
  guard (P.isDctorExported tyName exps ctorName)
  return $ H.renderHtml $ do
    para "decl" $ H.code $ do
      withClass "ident" . text . P.runProperName $ ctorName
      sp *> withClass "syntax" (text "::") <* sp
      typeToHtml fnType

  where
  fnType :: P.Type
  fnType = if null args
              then tyCtor
              else foldr1 P.function (args ++ [tyCtor])

  tyCtor :: P.Type
  tyCtor = P.TypeConstructor (P.Qualified Nothing tyName)

      

itemDocs :: Maybe [P.DeclarationRef] -> Item -> Maybe TL.Text
itemDocs exps (ItemDecl d)     = Just (declarationDocs exps d)
itemDocs exps (ItemDataCtor c) = dataConstructorDocs exps c
