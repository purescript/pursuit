# pursuit changelog

Please see https://github.com/purescript/pursuit/blob/master/CHANGELOG.md for
the most up-to-date version of this file.

## v0.3.10 (unreleased)

* Use the `barrier` Haskell library for generating SVG badges (#102,
  @tfausak).

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
