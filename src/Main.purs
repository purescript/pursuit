module Main where

import Data.Array
import Data.Maybe
import Data.Either
import Data.Tuple
import Data.Foreign
import Data.Foreign.Class
import Data.Foldable
import Data.Traversable

import Control.Monad.Eff
import Control.Monad.Eff.DOM
import Control.Monad.Eff.AJAX

import qualified Data.String as S
import qualified Data.Trie as T

data Entry = Entry String String String

instance isForeignEntry :: IsForeign Entry where
  read entry = Entry <$> readProp "module" entry
                     <*> readProp "name"   entry
                     <*> readProp "detail" entry

getQuery :: forall eff. Eff (dom :: DOM | eff) String
getQuery = do
  Just searchInput <- querySelector "#searchInput"
  query <- getValue searchInput

  return $ case readString query of
    Right s -> s
    Left _ -> ""

runSearch :: T.Trie Entry -> String -> Maybe [Tuple String Entry]
runSearch trie "" = Nothing
runSearch trie query = T.toArray <$> T.lookupAll (S.toLower query) trie

search :: forall eff. T.Trie Entry -> Eff (dom :: DOM | eff) Unit
search trie = do
  query <- getQuery

  maybeEl <- querySelector "#searchResults"

  case maybeEl of
    Nothing -> error "#searchResults not found"
    Just searchResults -> do
      setInnerHTML "" searchResults

      case runSearch trie query of
        Nothing -> return unit
        Just results -> do
          foreachE (take 20 results) $ \(Tuple _ (Entry moduleName name detail)) -> do
            div <- createElement "div"

            createElement "h2"
              >>= setText name
              >>= flip appendChild div
            createElement "div"
              >>= setText moduleName
              >>= flip appendChild div
            createElement "pre"
              >>= setText detail
              >>= flip appendChild div

            div `appendChild` searchResults
            return unit

foreign import error
  "function error(msg) {\
  \  throw new Error(msg);\
  \}":: forall a. String -> a

buildTrie :: String -> T.Trie Entry
buildTrie json = case parseJSON json >>= readArray >>= traverse read of
  Left err -> error $ show err
  Right arr -> foldl (\t (e@(Entry _ name _)) -> T.insert (S.toLower name) e t) T.empty (arr :: [Entry])

baseUrl :: forall eff. Eff (dom :: DOM | eff) String
baseUrl = do
  protocol <- locationProtocol
  host <- locationHost
  pathname <- locationPathname
  pure $ protocol ++ "//" ++ host ++ pathname

updateHistorySearch :: forall eff. Eff (dom :: DOM | eff) Unit
updateHistorySearch = do
  state <- historyState
  title <- documentTitle
  url <- baseUrl
  query <- getQuery
  replaceHistoryState state title $ url ++ "?" ++ query

main :: Eff (dom :: DOM, xhr :: XHR) Unit
main = do
  get "data.json" $ \json -> do
    maybeEl <- querySelector "#searchInput"

    case maybeEl of
      Nothing -> error "#searchInput not found"
      Just searchInput -> do
        let trie = buildTrie json
        for ["keyup", "change"] $ \evt -> do
          addEventListener evt (search trie) searchInput
          addEventListener evt updateHistorySearch searchInput
        query <- S.drop 1 <$> locationSearch
        setValue query searchInput
        search trie
