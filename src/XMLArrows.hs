module XMLArrows where

import Import
import Text.XML.HXT.Core as HXT
import Text.Blaze.Html (preEscapedToHtml)
import Text.Blaze.Renderer.String as BlazeS

run :: LA XmlTree XmlTree -> Html -> Html
run a = preEscapedToHtml . runString a . BlazeS.renderMarkup

runString :: LA XmlTree XmlTree -> String -> String
runString a =
  unsafeHead . runLA (hread >>> a >>> writeDocumentToString [])

-- | Remove all h1 elements.
stripH1 :: LA XmlTree XmlTree
stripH1 =
  processTopDown (neg (hasName "h1") `guards` this)

-- | Make all relative links into absolute links by providing a base URL.
makeRelativeLinksAbsolute ::
  String    -- ^ Tag name to modify
  -> String -- ^ Attribute name to modify
  -> String -- ^ Base URL to use for relative links
  -> LA XmlTree XmlTree
makeRelativeLinksAbsolute tagName attrName base =
  processTopDown $
    processAttrl (changeAttrValue (mkAbs base) `HXT.when` hasName attrName)
      `HXT.when` (isElem >>> hasName tagName)

  where
  mkAbs base' url = fromMaybe url $ expandURIString url $ base'

-- | Replace all <a> elements with <span>. We use this for rendering
-- documentation in search results, since each result is already wrapped in an
-- <a> element and browsers do not deal well with nested <a> elements (in fact,
-- they are invalid according to HTML5).
replaceLinks :: LA XmlTree XmlTree
replaceLinks =
  processTopDown $
    setElemName (mkName "span") `HXT.when` (isElem >>> hasName "a")

-- | Replace all <a> elements with relative URLs for href attributes with
-- <span> elements.
replaceRelativeLinks :: LA XmlTree XmlTree
replaceRelativeLinks =
  processTopDown $
    setElemName (mkName "span") `HXT.when` isRelative

  where
  isRelative :: LA XmlTree XmlTree
  isRelative =
    isElem
    >>> hasName "a"
    >>> filterA (getAttrValue "href" >>> isA isRelativeURI)

-- |
-- See Section 4.2 of RFC 3986:
-- https://tools.ietf.org/html/rfc3986#section-4.2
--
-- >>> isRelativeURI "http://example.com/" == False
-- >>> isRelativeURI "mailto:me@example.com" == False
-- >>> isRelativeURI "foo/bar" == True
-- >>> isRelativeURI "/bar" == True
-- >>> isRelativeURI "./bar" == True
--
isRelativeURI :: String -> Bool
isRelativeURI =
  takeWhile (/= '/')
  >>> (\segment -> ':' `notElem` segment)
