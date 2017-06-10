-- | Common handler functions.
module Handler.Common where

import Data.Aeson (Value)
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import Data.FileEmbed (embedFile)
import Yesod.Core (renderRoute)
import Yesod.Core.Json (returnJson)
import Import

-- Create a Web Application Manifest as defined by the W3C. We use this to declare the
-- "Add to home screen" icons and other settings for Android Chrome.

data Icon = Icon {ic_src :: Text, ic_sizes :: String, ic_type :: String}
$(deriveJSON defaultOptions{fieldLabelModifier = drop 3} ''Icon)

data Manifest = Manifest {mf_name :: String, mf_theme_color :: String, mf_display :: String, mf_icons :: [Icon]}
$(deriveJSON defaultOptions{fieldLabelModifier = drop 3} ''Manifest)

getManifestR :: Handler Value
getManifestR = returnJson $ Manifest
  { mf_name = "Pursuit"
  , mf_theme_color = "#aaaaaa"
  , mf_display = "standalone"
  , mf_icons = [ Icon { ic_src = (path favicon_android_chrome_192x192_png)
                      , ic_sizes = "192x192"
                      , ic_type = "image/png"
                      }
               , Icon { ic_src = (path favicon_android_chrome_512x512_png)
                      , ic_sizes = "512x512"
                      , ic_type = "image/png"
                      }
               ]}
  where
    path :: RenderRoute a => Route a -> Text
    path = (mappend $ pack "/static/") . (intercalate "/") . fst . renderRoute


-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")
