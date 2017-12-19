-- |
-- This module takes care of rendering the Markdown help docs to HTML, and
-- embedding them into the `pursuit` binary at compile-time.
--
module EmbeddedDocs
  ( helpIndex
  -- , helpAuthors
  -- , helpUsers
  ) where

import Import.NoFoundation
import Data.FileEmbed (embedFile)
import qualified Cheapskate

renderMarkdown :: ByteString -> Html
renderMarkdown = toHtml . Cheapskate.markdown def . decodeUtf8

helpIndex :: Html
helpIndex = renderMarkdown $(embedFile "static/help-docs/index.md")
