# Pursuit Changelog

Please see https://github.com/purescript/pursuit/blob/master/CHANGELOG.md for
the most up-to-date version of this file.

## Unreleased

- Update `purescript` to `0.14.5` and `purescript-cst` to `0.4.0.0` (@JordanMartinez)
- 

## v0.8.5

- Fix build command used in CI (@thomashoneyman)

## v0.8.4

- Fix typo in CI release script (@thomashoneyman)

## v0.8.3

- Build against v0.14.3 of the PureScript compiler (#436 by @thomashoneyman)
- Migrate to GitHub Actions for CI (#435 by @thomashoneyman)
- Fix an internal typo (#432 by @i-am-the-slime)
- Added a section on kind signatures (#437 by @JordanMartinez)

## v0.8.2

- Default `constraintKindArgs` to an empty list (#430 by @thomashoneyman)

## v0.8.1

- Update outdated Pursuit version in the .cabal file to 0.8.1 (@thomashoneyman)

## v0.8.0

- Build against release 0.14.0-rc3 of the PureScript compiler (#428 by @thomashoneyman)

## v0.7.7

- Avoid deleting the .git directory in the backup script (@hdgarrood)

## v0.7.6

- Fix backup script (@hdgarrood)

## v0.7.5

- Bump the tag this for release via CI (@hdgarrood)

## v0.7.4

- Include the backup script in the deploy bundle (@hdgarrood)

## v0.7.3

- Build against version 0.13.0 of the PureScript compiler (@hdgarrood)

## v0.7.2

- Fix a bug where Prim submodules did not show up in search results (#387)
- Fix a bug where the `text` field for JSON search results contained HTML, as
  opposed to the raw Markdown text present in the source file (#171)
- Recommend viewing docs locally before publishing (#384)

## v0.7.1

- Build against version 0.12.2 of the PureScript compiler (@hdgarrood)

## v0.7.0

- Build against version 0.12.0 of the PureScript compiler (@hdgarrood)
- Make documentation for all builtin modules available, rather than just Prim
  (@hdgarrood, #357)
- Fix a bug where incorrect types were displayed in search results for data
  constructors with 2 or more arguments (@jonathanlking, #373)
- Add an OpenSearch description file (@hdgarrood, #325)
- Use smart quotes in help pages (@hdgarrood, 358)

## v0.6.3

- Add a "load more" button for display of additional search results
  (@felixschl, #305)
- Fix source links in re-exported declarations (@felixschl, #345)
- Display the types of data constructors and type class members in search
  results (@hdgarrood, #264)
- Include entries from `Prim` in search results (@hdgarrood, #265)
- Allow data constructors and type class members to be found when searching by
  type (@hdgarrood, #303)
- Fix searching for type operators by name (@hdgarrood, #330)
- Slightly promote search results which come from packages with more reverse
  dependencies (@hdgarrood, #353)
- Fix an issue where any query which included a hyphen would be treated as a
  type, preventing searching by name; this was a problem for queries such as
  e.g. "generics-rep" (@hdgarrood, #321)
- Take into account whether type variables match when performing type search.
  For example, after this change, `map` ranks higher than `cmap` for the query
  "(a -> b) -> f a -> f b"; previously they were equally ranked (@hdgarrood,
  #355)
- Add help explaining the search feature (@hdgarrood / @grossbart, #339, #357)

## v0.6.2

- Improve 'package not found' message (@hdgarrood)
- Add favicons (@grossbart)
- Update to PureScript 0.11.7 (@hdgarrood)
- Add publish date to package pages (@hdgarrood)
- Redirect URLs without a version to the latest version (@hdgarrood)
- Strip leading/trailing whitespace before searching (@hdgarrood)
- Allow operators to appear in search results without being wrapped by parens
- (@hdgarrood)
- Add a link to Prim to the homepage (@joneshf)
- Clarify contributing docs (@grossbart)
- Update to stackage LTS 8.18 (@hdgarrood)

## v0.6.1

- Update to `purescript-0.11.4`.

## v0.6.0

- Update `aeson` and `purescript` dependencies for JSON compatibility.

## v0.5.0

- Update to `purescript-0.11.0`.

## v0.4.\*

- Recommend pulp for uploading packages (@hdgarrood)
- Remove the package upload form (@hdgarrood)
- Update for purescript 0.9.x (@jplatte)
- Switch to using an in-memory trie instead of Hoogle for searching (@paf31)
- Group packages by letter (@paf31)
- Add list of latest uploads to homepage (@paf31)
- Include type class members when searching by name (@paf31)
- Add the types of things to search results (@nwolversion)
- Explain what happened when failing to fetch a README (@hdgarrood)
- Changes to help with building with GHC8 and Nix (@abuibrahim)
- Better handling of invalid uploaded gzip data (@hdgarrood)
- Provide Prim docs (@hdgarrood)
- Incredible redesign (@grossbart)
- Some styling fixes (@utatti)
- Improve compareTypes algorithm (@matthewleon)

## v0.3.10

- Update to purescript-0.8.5.
- Use the `barrier` Haskell library for generating SVG badges (#102,
  @tfausak).
- Improve error messages when uploading incompatible JSON data from a newer
  version of the compiler.
- Invalidate caches for `/` and `/packages` properly (#167).

## v0.3.9

- Update to purescript-0.8.4. Fixes an issue where types with constraints
  not in the left-most position were being rendered incorrectly: for
  example, `unsafePartial` (#176).

## v0.3.8

- Remove superfluous "licensed" text on package pages. ("MIT licensed
  licensed")
- Reject packages which do not have a license.

## v0.3.7

- Re-exported declarations are now included on documentation pages.
- Fix versions being ordered nonsensically on 'No such version' pages (#201,
  @stefanholzmueller).
- Improve startup time by not waiting for the Hoogle database generation to
  complete (#108).

## v0.3.6

- Keep search queries in the search field, allowing you to e.g. easily edit in
  case of typos (#185, @LiamGoodacre).
- Fix links to primitive types (#177, @LiamGoodacre).
- Fix parse errors when searching for operators without parentheses (#175,
  @kseo).
- Fix display of module names in link tooltips (#202, @LiamGoodacre).
- Update to v0.8.3 of the compiler library.
- Update Stackage snapshot: lts-5.4 → lts-5.10.

## v0.3.5

- Fix links in search results to declarations with non-URL-safe characters
  (#166, @kseo).

## Earlier versions

Versions before v0.3.5 are not included, sorry.
