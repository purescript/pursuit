-- |
-- This module takes care of rendering the Markdown help docs to HTML, and
-- embedding them into the `pursuit` binary at compile-time.
--
module EmbeddedDocs
  ( helpIndex
  , helpAuthors
  , helpUsers
  ) where

import Import.NoFoundation
import Data.FileEmbed (embedFile)
import qualified Cheapskate
import Cheapskate.SmartQuotes (smartQuotes, defaultQuoteStyle)

renderMarkdown :: ByteString -> Html
renderMarkdown =
  toHtml
  . smartQuotes defaultQuoteStyle
  . Cheapskate.markdown def
  . decodeUtf8

helpIndex :: Html
helpIndex = renderMarkdown $(embedFile "static/help-docs/index.md")

helpAuthors :: Html
helpAuthors = renderMarkdown $(embedFile "static/help-docs/authors.md")

helpUsers :: Html
helpUsers = renderMarkdown $(embedFile "static/help-docs/users.md")
