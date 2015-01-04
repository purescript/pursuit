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
import Control.Monad.Eff.AJAX
import Control.Monad.Eff.History

import qualified Thermite as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as T
import qualified Thermite.Html.Attributes as A
import qualified Thermite.Action as T
import qualified Thermite.Events as T
import qualified Thermite.Types as T

data Entry = Entry String String String

instance isForeignEntry :: IsForeign Entry where
  read entry = Entry <$> readProp "module" entry
                     <*> readProp "name"   entry
                     <*> readProp "detail" entry
  
data Action = Search String

type State = { results :: [Entry] }

initialState :: State
initialState = { results: [] }  
      
foreign import getValue 
  "function getValue(e) {\
  \  return e.target.value;\
  \}" :: T.FormEvent -> String
      
handleOnChangeEvent :: T.FormEvent -> Action
handleOnChangeEvent = Search <<< getValue
        
render :: T.Render State _ Action
render ctx s _ = container [ header [ T.h1' [ T.text "Pursuit" ]
                                    , T.div' [ T.input [ A._type "search"
                                                       , A.className "form-control"
                                                       , A.placeholder "Search..."
                                                       , T.onChange ctx handleOnChangeEvent
                                                       , A.autoFocus true
                                                       ] []
                                             ]
                                    ]
                           , body   [ T.div' (searchResult <$> s.results)
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

  searchResult :: Entry -> T.Html _
  searchResult (Entry moduleName name detail) = 
    T.div' [ T.h2'  [ T.code' [ T.text name ] ]
           , T.div' [ T.code' [ T.text moduleName ] ]
           , T.pre' [ T.text detail ]
           ]

performAction :: T.PerformAction _ Action (T.Action _ State) 
performAction _ (Search q) = do
  T.sync $ updateHistorySearch q  
  
  let uri = "/search?q=" <> q
  json <- T.async $ get uri
  
  T.setState { results: case parseJSON json >>= read of
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
spec = T.Spec { initialState: initialState
              , performAction: performAction
              , render: render
              }

main = do
  let component = T.createClass spec
  T.render component {}
