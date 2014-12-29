module Control.Monad.Eff.History where
  
import Control.Monad.Eff

foreign import data History :: !

foreign import locationProtocol
  "function locationProtocol() {\
  \  return window.location.protocol;\
  \}" :: forall eff. Eff (history :: History | eff) String

foreign import locationHost
  "function locationHost() {\
  \  return window.location.host;\
  \}" :: forall eff. Eff (history :: History | eff) String

foreign import locationPathname
  "function locationPathname() {\
  \  return window.location.pathname;\
  \}" :: forall eff. Eff (history :: History | eff) String

foreign import locationSearch
  "function locationSearch() {\
  \  return window.location.search;\
  \}" :: forall eff. Eff (history :: History | eff) String

foreign import pushHistoryState
  "function pushHistoryState(data) {\
  \  return function(title) {\
  \    return function(url) {\
  \      return function() {\
  \        window.history.pushState(data, title, url);\
  \      };\
  \    };\
  \  };\
  \}" :: forall eff s. { | s } -> String -> String -> Eff (history :: History | eff) Unit

foreign import replaceHistoryState
  "function replaceHistoryState(data) {\
  \  return function(title) {\
  \    return function(url) {\
  \      return function() {\
  \        window.history.replaceState(data, title, url);\
  \      };\
  \    };\
  \  };\
  \}" :: forall eff s. { | s } -> String -> String -> Eff (history :: History | eff) Unit