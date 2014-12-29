module Control.Monad.Eff.AJAX where
  
import Control.Monad.Eff

foreign import data XHR :: !

type URI = String

foreign import get 
  "function get(uri) {\
  \  return function(k) {\
  \    return function() {\
  \      var req = new XMLHttpRequest();\
  \      req.onreadystatechange = function() {\
  \        if (req.readyState === 4 && req.status === 200) {\
  \          k(req.responseText)();\
  \        }\
  \      };\
  \      req.open('GET', uri, true);\
  \      req.send();\
  \    };\
  \  };\
  \}" :: forall eff. URI -> (String -> Eff (xhr :: XHR | eff) Unit) -> Eff (xhr :: XHR | eff) Unit 
