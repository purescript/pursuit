module Cheapskate.SmartQuotes
  ( QuoteStyle(..)
  , defaultQuoteStyle
  , smartQuotes
  ) where

import Prelude
import Data.Text (Text)
import Data.Sequence (Seq, (<|), (|>))
import qualified Data.Sequence as Seq
import Cheapskate (Doc(..), Inline(..), Block(Para), walk)

data QuoteStyle = QuoteStyle
  { quoteSingle :: (Text, Text)
  , quoteDouble :: (Text, Text)
  }
  deriving (Show, Eq, Ord)

defaultQuoteStyle :: QuoteStyle
defaultQuoteStyle = QuoteStyle
  { quoteSingle = ("‘", "’") -- U+2018, U+2019
  , quoteDouble = ("“", "”") -- U+201C, U+201D
  }

-- | Convert straight quotes to smart quotes, according to the given quote
-- style, in the given 'Doc'.
smartQuotes :: QuoteStyle -> Doc -> Doc
smartQuotes style = walk smartly
  -- A paragraph in Cheapskate is represented by a list of Inline elements, one
  -- of which is the 'Str' constructor, which takes a single 'Text' argument.
  -- It is important to note that any character which is not ascii and
  -- alphanumeric is placed in a 'Str' constructor on its own. For example,
  -- the input 'hello' comes through with three 'Str' constructors: one for the
  -- first single quote, one for the string "hello", and a third for the ending
  -- single quote.
  where
  smartly (Para inlines) = Para (processWindows go inlines)
  smartly other = other

  go :: Window Inline -> Inline
  go w =
    case curr w of
      Str "\"" ->
        Str (quoteDirection w (quoteDouble style))
      Str "'" ->
        Str (quoteDirection w (quoteSingle style))
      other ->
        other

-- | True if the given inline element counts as space for the purposes of
-- determining quote direction.
countsAsSpace :: Inline -> Bool
countsAsSpace = \case
  Space -> True
  SoftBreak -> True
  LineBreak -> True
  _ -> False

-- | Determines whether we should use the left or right quote mark. This
-- function returns 'fst' where we should use the left quote, and 'snd' where
-- we should use the right quote.
quoteDirection :: Window Inline -> ((a,a) -> a)
quoteDirection window =
  let
    space = maybe True countsAsSpace
  in
    case (space (prev window), space (next window)) of
      (True, True) ->
        -- on its own; the choice of what to do here seems kind of arbitrary.
        snd
      (True, False) ->
        -- probably at the start of a quotation
        fst
      (False, True) ->
        -- probably at the end of a quotation
        snd
      (False, False) ->
        -- probably internal to a word, e.g. "don't". Use a right quote
        -- (apparently that's what we are supposed to use for apostrophes).
        snd

data Window a = Window
  { prev :: Maybe a
  , curr :: a
  , next :: Maybe a
  }
  deriving (Show, Eq, Ord)

windows :: Seq a -> Seq (Window a)
windows xs =
  Seq.zipWith3 Window
    (Nothing <| fmap Just xs)
    xs
    (fmap Just (Seq.drop 1 xs) |> Nothing)

processWindows :: (Window a -> a) -> Seq a -> Seq a
processWindows f = fmap f . windows
