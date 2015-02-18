module Main where

import Data.Array
import Data.Maybe
import Data.Either
import Data.Tuple
import Data.Foreign
import Data.Foreign.Class
import Data.Foldable
import Data.Traversable

import qualified Data.String as S

import Control.Monad.Eff
import Control.Monad.Eff.AJAX
import Control.Monad.Eff.History

import qualified Thermite as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as T
import qualified Thermite.Html.Attributes as A
import qualified Thermite.Action as T
import qualified Thermite.Events as T
import qualified Thermite.Types as T

data Entry = Entry String String String String

instance isForeignEntry :: IsForeign Entry where
  read entry = Entry <$> readProp "library" entry
                     <*> readProp "module" entry
                     <*> readProp "name"   entry
                     <*> readProp "detail" entry
  
data Action = Change String | Search String | ReadQueryString | DoNothing

type State = { lastSearch :: String
             , query :: String
             , results :: [Entry] 
             }

initialState :: State
initialState = { lastSearch: "", query: "", results: [] }  
      
foreign import getValue 
  "function getValue(e) {\
  \  return e.target.value;\
  \}" :: forall event. event -> String

foreign import getKeyCode
  "function getKeyCode(e) {\
  \  return e.keyCode;\
  \}" :: T.KeyboardEvent -> Number

handleChange :: T.FormEvent -> Action
handleChange e = Change (getValue e)

handleKey :: T.KeyboardEvent -> Action
handleKey e = case getKeyCode e of
                13 -> Search $ getValue e
                _  -> DoNothing

handleBlur :: T.FocusEvent -> Action
handleBlur e = Search $ getValue e
        
render :: T.Render State _ Action
render ctx s _ = container [ header [ T.h1' [ T.text "Pursuit" ]
                                    , T.div' [ T.input [ A._type "search"
                                                       , A.className "form-control"
                                                       , A.placeholder "Search..."
                                                       , T.onChange ctx handleChange
                                                       , T.onBlur   ctx handleBlur
                                                       , T.onKeyUp  ctx handleKey
                                                       , A.autoFocus true
                                                       , A.value s.query
                                                       ] []
                                             ]
                                    ]
                           , body   [ searchResults
                                    , T.div' [ T.a [ A.href "http://github.com/purescript/pursuit" ] [ T.text "Source" ]
                                             , T.text " | " 
                                             , T.a [ A.href "http://purescript.org" ] [ T.text "PureScript" ]
                                             ]
                                    ]
                           ]
  where
  container = T.div [ A.className "container-fluid" ]
  
  header = T.div [ A.className "header" ]
      
  body = T.div [ A.className "body" ]

  searchResults :: T.Html _
  searchResults | s.lastSearch == "" = T.p' [ T.text "Enter a search term above." ]
                | null s.results = T.p' [ T.text $ "No results for '" <> s.lastSearch <> "'" ]
                | otherwise = T.div' (searchResult <$> s.results)

  searchResult :: Entry -> T.Html _
  searchResult (Entry libraryName moduleName name detail) = 
    T.div' [ T.h2'  [ T.code' [ T.text name ] ]
           , T.div' [ T.code' [ T.text moduleName ], T.text " (", T.code' [ T.text libraryName ], T.text ")" ]
           , T.pre' [ T.text detail ]
           ]

performAction :: T.PerformAction _ Action (T.Action _ State) 
performAction _ (Change s) = T.modifyState \o -> o { query = s }
performAction _ (Search "") = T.setState { query: "", lastSearch: "", results: [] }
performAction _ (Search q) = do
  T.sync $ updateHistorySearch q  
  search q
performAction _ ReadQueryString = do
  q <- S.drop 1 <$> T.sync locationSearch
  T.setState { query: q, lastSearch: q, results: [] }
  case q of
    "" -> return unit
    _  -> search q     
performAction _ DoNothing = return unit

search :: String -> T.Action _ State Unit
search q = do
  let uri = "/search?q=" <> q
  json <- T.async $ get uri

  T.setState { query: q
             , lastSearch: q
             , results: case parseJSON json >>= read of
                          Left _ -> []
                          Right results -> results 
             }

baseUrl :: forall eff. Eff (history :: History | eff) String
baseUrl = do
  protocol <- locationProtocol
  host     <- locationHost
  pathname <- locationPathname
  pure $ protocol ++ "//" ++ host ++ pathname

updateHistorySearch :: forall eff. String -> Eff (history :: History | eff) Unit
updateHistorySearch query = do
  url <- baseUrl
  replaceHistoryState {} "PURSuit" $ url ++ "?" ++ query

spec :: T.Spec _ State _ Action
spec = T.simpleSpec initialState performAction render
         # T.componentWillMount ReadQueryString

main = do
  let component = T.createClass spec
  T.render component {}
