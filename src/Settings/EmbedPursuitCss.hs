-- |
-- This module exists in order to include the pursuit CSS (which comes from the
-- purescript library) within the embedded static files known to Yesod.
--
-- Template Haskell stage restrictions necessitate putting this code into a
-- module separate from the Settings module.
--
module Settings.EmbedPursuitCss (pursuitCssEntry) where

import ClassyPrelude.Yesod
import Language.Haskell.TH (mkName)
import Language.PureScript.Docs (pursuitCss)
import Yesod.EmbeddedStatic.Types (Entry(..))

pursuitCssEntry :: Entry
pursuitCssEntry =
  def { ebHaskellName = Just (mkName "css_pursuit_css")
      , ebLocation = "css/pursuit.css"
      , ebMimeType = "text/css"
      , ebProductionContent = pure (fromStrict pursuitCss :: LByteString)
      , ebDevelReload = [|pure (fromStrict pursuitCss :: LByteString)|]
      }
