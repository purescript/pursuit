module Control.Monad.Eff.DOM where

import Data.Maybe
import Data.Foreign
import Data.Function

import Control.Monad.Eff

foreign import data DOM :: !

foreign import data Node :: *

--
-- Creating Nodes
--

foreign import body
  "function body() {\
  \  return document.body;\
  \}" :: forall eff. Eff (dom :: DOM | eff) Node

foreign import createElement
  "function createElement(name) {\
  \  return function() {\
  \    return document.createElement(name);\
  \  };\
  \}" :: forall eff. String -> Eff (dom :: DOM | eff) Node

--
-- Selector Functions
--

foreign import querySelectorImpl
  "function querySelectorImpl(r, f, s) {\
  \  return function() {\
  \    var result = document.querySelector(s);\
  \    return result ? f(result) : r;\
  \  };\
  \}" :: forall eff r. Fn3 r (Node -> r) String (Eff (dom :: DOM | eff) r)

querySelector :: forall eff. String -> Eff (dom :: DOM | eff) (Maybe Node)
querySelector s = runFn3 querySelectorImpl Nothing Just s

--
-- Append
--

foreign import appendChild
  "function appendChild(child) {\
  \  return function(node) {\
  \    return function() {\
  \      node.appendChild(child);\
  \      return node;\
  \    };\
  \  };\
  \}" :: forall eff. Node -> Node -> Eff (dom :: DOM | eff) Node

--
-- Styles
--

foreign import addClass
  "function addClass(className) {\
  \  return function(node) {\
  \    return function() {\
  \      node.classList.add(className);\
  \      return node;\
  \    };\
  \  };\
  \}" :: forall eff. String -> Node -> Eff (dom :: DOM | eff) Node

--
-- Properties
--

foreign import setText
  "function setText(text) {\
  \  return function(node) {\
  \    return function() {\
  \      node.textContent = text;\
  \      return node;\
  \    };\
  \  };\
  \}" :: forall eff. String -> Node -> Eff (dom :: DOM | eff) Node

foreign import setValue
  "function setValue(text) {\
  \  return function(node) {\
  \    return function() {\
  \      node.value = text;\
  \      return node;\
  \    };\
  \  };\
  \}" :: forall eff. String -> Node -> Eff (dom :: DOM | eff) Node

foreign import getValue
  "function getValue(node) {\
  \  return function() {\
  \    return node.value;\
  \  };\
  \}" :: forall eff. Node -> Eff (dom :: DOM | eff) Foreign

foreign import setInnerHTML
  "function setInnerHTML(html) {\
  \  return function(node) {\
  \    return function() {\
  \      node.innerHTML = html;\
  \      return node;\
  \    };\
  \  };\
  \}" :: forall eff. String -> Node -> Eff (dom :: DOM | eff) Node

--
-- Events
--

foreign import addEventListener
  "function addEventListener(name) {\
  \  return function(handler) {\
  \    return function(node) {\
  \      return function() {\
  \        node.addEventListener(name, function(e) {\
  \          handler();\
  \        });\
  \      };\
  \    };\
  \  };\
  \}" :: forall eff. String -> Eff (dom :: DOM | eff) Unit -> Node -> Eff (dom :: DOM | eff) Node

--
-- Document
--

foreign import documentTitle
  "function documentTitle() {\
  \  return document.title;\
  \}" :: forall eff. Eff (dom :: DOM | eff) String

--
-- Location
--

foreign import locationProtocol
  "function locationProtocol() {\
  \  return window.location.protocol;\
  \}" :: forall eff. Eff (dom :: DOM | eff) String

foreign import locationHost
  "function locationHost() {\
  \  return window.location.host;\
  \}" :: forall eff. Eff (dom :: DOM | eff) String

foreign import locationPathname
  "function locationPathname() {\
  \  return window.location.pathname;\
  \}" :: forall eff. Eff (dom :: DOM | eff) String

foreign import locationSearch
  "function locationSearch() {\
  \  return window.location.search;\
  \}" :: forall eff. Eff (dom :: DOM | eff) String

--
-- History
--

foreign import historyState
  "function historyState() {\
  \  return window.history.state;\
  \}" :: forall eff s. Eff (dom :: DOM | eff) { | s }

foreign import pushHistoryState
  "function pushHistoryState(data) {\
  \  return function(title) {\
  \    return function(url) {\
  \      return function() {\
  \        window.history.pushState(data, title, url);\
  \      };\
  \    };\
  \  };\
  \}" :: forall eff s. { | s } -> String -> String -> Eff (dom :: DOM | eff) Unit

foreign import replaceHistoryState
  "function replaceHistoryState(data) {\
  \  return function(title) {\
  \    return function(url) {\
  \      return function() {\
  \        window.history.replaceState(data, title, url);\
  \      };\
  \    };\
  \  };\
  \}" :: forall eff s. { | s } -> String -> String -> Eff (dom :: DOM | eff) Unit
