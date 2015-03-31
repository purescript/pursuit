module Prim where

-- | A JavaScript function, which takes values of the first type, and
-- | returns values of the second.
-- |
-- | The type constructor `(->)` is syntactic sugar for this type constructor.
-- | It is recommended to use `(->)` rather than `Function`, where possible.
-- |
-- | That is, prefer this:
-- |
-- | * `f :: Number -> Number`
-- |
-- | to either of these:
-- |
-- | * `f :: Function Number Number`
-- | * `f :: (->) Number Number`
foreign import data Function :: * -> * -> *

-- | A JavaScript Array. PureScript provides syntactic sugar for this type
-- | constructor: `[a]` desugars to `Array a`. However, this syntax is
-- | deprecated and will be going away soon.
foreign import data Array :: *

-- | A JavaScript Object.
-- |
-- | The type signature here means that the `Object` type constructor takes
-- | a row of concrete types. For example:
-- |
-- | * `type Person = Object (name :: String, age :: Number)`
-- |
-- | The syntactic sugar with curly braces `{ }` is generally preferred:
-- |
-- | * `type Person = { name :: String, age :: Number }`
foreign import data Object :: # * -> *

-- | A JavaScript Number: double precision floating point (IEEE 754).
-- |
-- | Construct values of this type with literals:
-- | * `x = 35 :: Number`
-- | * `y = 35.23 :: Number`
-- | * `z = 1.224e6 :: Number`
foreign import data Number :: *

-- | A JavaScript String.
-- |
-- | Construct values of this type with literals:
-- | * `x = "hello, world" :: String`
-- |
-- | Multi-line string literals are also supported with triple quotes (`"""`).
foreign import data String :: *

-- | A JavaScript Boolean value.
-- |
-- | Construct values of this type with the literals `true` and `false`.
foreign import data Boolean :: *
