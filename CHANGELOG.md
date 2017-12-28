# pursuit changelog

Please see https://github.com/purescript/pursuit/blob/master/CHANGELOG.md for
the most up-to-date version of this file.

## v0.6.3

* Add a "load more" button for display of additional search results
  (@felixschl, #305)
* Fix source links in re-exported declarations (@felixschl, #345)
* Display the types of data constructors and type class members in search
  results (@hdgarrood, #264)
* Include entries from `Prim` in search results (@hdgarrood, #265)
* Allow data constructors and type class members to be found when searching by
  type (@hdgarrood, #303)
* Fix searching for type operators by name (@hdgarrood, #330)
* Slightly promote search results which come from packages with more reverse
  dependencies (@hdgarrood, #353)
* Fix an issue where any query which included a hyphen would be treated as a
  type, preventing searching by name; this was a problem for queries such as
  e.g. "generics-rep".
* Take into account whether type variables match when performing type search.
  For example, after this change, `map` ranks higher than `cmap` for the query
  "(a -> b) -> f a -> f b"; previously they were equally ranked.
* Add help explaining the search feature (@hdgarrood / @grossbart, #339, #357)

## v0.6.2

* Improve 'package not found' message (@hdgarrood)
* Add favicons (@grossbart)
* Update to PureScript 0.11.7 (@hdgarrood)
* Add publish date to package pages (@hdgarrood)
* Redirect URLs without a version to the latest version (@hdgarrood)
* Strip leading/trailing whitespace before searching (@hdgarrood)
* Allow operators to appear in search results without being wrapped by parens
* (@hdgarrood)
* Add a link to Prim to the homepage (@joneshf)
* Clarify contributing docs (@grossbart)
* Update to stackage LTS 8.18 (@hdgarrood)

## v0.6.1

* Update to `purescript-0.11.4`.

## v0.6.0

* Update `aeson` and `purescript` dependencies for JSON compatibility.

## v0.5.*

* A bunch of stuff happened, I forgot to update the changelog, and it doesn't
  really seem worth going through the commit history at this point.

## v0.4.*

* A bunch of stuff happened, I forgot to update the changelog, and it doesn't
  really seem worth going through the commit history at this point.

## v0.3.10

* Update to purescript-0.8.5.
* Use the `barrier` Haskell library for generating SVG badges (#102,
  @tfausak).
* Improve error messages when uploading incompatible JSON data from a newer
  version of the compiler.
* Invalidate caches for `/` and `/packages` properly (#167).

## v0.3.9

* Update to purescript-0.8.4. Fixes an issue where types with constraints
  not in the left-most position were being rendered incorrectly: for
  example, `unsafePartial` (#176).

## v0.3.8

* Remove superfluous "licensed" text on package pages. ("MIT licensed
  licensed")
* Reject packages which do not have a license.

## v0.3.7

* Re-exported declarations are now included on documentation pages.
* Fix versions being ordered nonsensically on 'No such version' pages (#201,
  @stefanholzmueller).
* Improve startup time by not waiting for the Hoogle database generation to
  complete (#108).

## v0.3.6

* Keep search queries in the search field, allowing you to e.g. easily edit in
  case of typos (#185, @LiamGoodacre).
* Fix links to primitive types (#177, @LiamGoodacre).
* Fix parse errors when searching for operators without parentheses (#175,
  @kseo).
* Fix display of module names in link tooltips (#202, @LiamGoodacre).
* Update to v0.8.3 of the compiler library.
* Update Stackage snapshot: lts-5.4 → lts-5.10.

## v0.3.5

* Fix links in search results to declarations with non-URL-safe characters
  (#166, @kseo).

## Earlier versions

Versions before v0.3.5 are not included, sorry.
