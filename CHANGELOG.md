# Pursuit Changelog

Please see https://github.com/purescript/pursuit/blob/master/CHANGELOG.md for
the most up-to-date version of this file.

## Unreleased

- Challenge browser-like scraper traffic to package pages with Anubis
  (@thomashoneyman)

  Distributed scraper fleets (thousands of IPs presenting spoofed browser
  user agents, so robots.txt does not apply) crawl the module documentation
  pages of large generated packages hard enough to take the server down.
  Deploys now install [Anubis](https://github.com/TecharoHQ/anubis), a
  challenge proxy, and nginx routes cache misses under `/packages/` through
  it: clients with browser-like user agents must pass a JavaScript
  proof-of-work challenge once, while non-browser clients (curl, package
  uploads, editor tooling, badge fetchers) and known-good search engine
  crawlers pass through untouched. Cached package pages, `/search`, and all
  other routes bypass Anubis entirely.

- Serve cached pages to browsers, and treat HEAD like GET (@thomashoneyman)

  The nginx Accept-header map had no default, so the composite Accept
  strings real browsers send ("text/html,application/xhtml+xml,...") never
  matched the page cache, and every browser page view was re-rendered by the
  backend. Cache lookups now default to the HTML representation, making
  cache hits real for browsers - and confining the Anubis detour above to
  genuine cache misses. HEAD requests now take the same cache/challenge path
  as GET instead of going straight to the backend, where Yesod runs the full
  GET handler for them - previously a full-cost decode that looked free.

- Give large package decodes an aggregate memory budget (@thomashoneyman)

  v0.9.11 serialised decodes of package docs JSON files of 5MB or more, but
  concurrent decodes of files just below that cutoff could still exhaust the
  heap: scrapers crawling the ~1,900 module pages of next-purs-rsc (a 4.4MB
  file) repeatedly took the server down by stacking four or five decodes of
  it at once. Decodes of files of 1MB or more now share a 16MB in-flight
  budget instead: a decode starts once the total size of large files being
  decoded fits within the budget (or immediately if it is the only large
  decode running, so files bigger than the budget itself are still served).
  The queue remains bounded, failing fast with a 503 when full, and the
  search index regeneration remains exempt from the bound.

  Budget admission is first-come-first-served: without an ordering, a file
  too large to share the budget (which must wait until nothing else is in
  flight) can be overtaken indefinitely by a steady stream of smaller
  decodes - load testing showed a crawler burst starving the search index
  build behind a react-icons decode for over an hour.
  
- Type search queries containing type variables now also match more concrete
  types: `a -> HTMLElement` finds `HTMLAnchorElement -> HTMLElement` the same
  way `_ -> HTMLElement` does (#395). Unlike a wildcard, instantiating a query
  variable charges a small penalty, so results that unify with the query
  directly rank first, and repeated variables must be instantiated
  consistently (`a -> a` ranks `Int -> Int` above `Int -> String`). Based on
  #396 by @klntsky. (@thomashoneyman)
  
- The package and module badges on search results are now links to the
  package page and module docs page (#424, @joprice). Builtin modules such
  as Prim have no package page, so their package badge remains plain text.
- Write cached responses atomically (@thomashoneyman)

  Every request that renders a cacheable page writes the response body to
  the same cache file, so concurrent requests for one page raced on it: the
  losers failed with "resource busy (file is locked)" and were served a 500
  (#482 - under a 400-connection homepage flood, a third of responses), and
  a half-written file was briefly visible to nginx, which serves the cache
  directory directly. Responses are now written to a temporary file and
  renamed into place, which is atomic; the same flood now returns no errors.

## v0.9.11

- Serialise decoding of large package files (@thomashoneyman)

  Rendering any documentation page decodes the entire docs JSON for that
  package version, which transiently needs tens of times the file's size in
  heap. A few generated packages (react-icons, elmish-html, deku, ...) have
  files of 10MB or more with enormous numbers of rarely-cached documentation
  pages, so a handful of concurrent crawler requests for those pages could
  exhaust the heap and restart the server. Decodes of files over 5MB now run
  one at a time; smaller packages (nearly all of them) are unaffected. The
  file is only read once the lock is held, so queued requests do not pin file
  contents, and the queue is bounded (excess requests fail fast with a 503
  rather than accumulating while clients time out). The hourly index
  regeneration is exempt from the bound so the index never silently omits a
  package.

- Do not decode a package version's file twice when rendering the package
  page for the latest version (@thomashoneyman)
- Disallow SEO/bulk crawlers (Semrush, Ahrefs, DotBot, MJ12, Amazonbot,
  Bytespider, PetalBot) in robots.txt (@thomashoneyman)
- Evict page-cache files not accessed in 90 days via a weekly cron job
  installed by the deploy, so the cache cannot fill the disk
  (@thomashoneyman)
- Raise the heap limit to `-M3400m` now that the server has swap to absorb
  transient spikes above physical RAM (@thomashoneyman)
- Add `deploy/SERVER.md` documenting server-only and DigitalOcean account
  state (@thomashoneyman)

## v0.9.10

- Build the search index one package at a time (@thomashoneyman)

  Previously the hourly search index regeneration (which also runs at startup)
  decoded every package in the database into memory at once before building
  the index. The decoded packages now require more memory than the server has,
  which caused the server to be OOM-killed several times an hour. Packages are
  now decoded and indexed one at a time, so only the index entries themselves
  are retained.

  A package file which cannot be read or decoded is now skipped (previously it
  silently aborted the entire index build, leaving the search index empty),
  and the new index is fully evaluated on the regeneration thread before being
  published, rather than by the first search request to use it.

- Use both CPU cores and cap the heap below available RAM (`-N2 -A64m -M3G`)
  in `pursuit.service` (@thomashoneyman)
- Update GitHub Actions to current action versions and pin the runner to
  `ubuntu-24.04` to match the server (@thomashoneyman)

## v0.9.9

- Update pursuit.service to 3.5GB max memory (@thomashoneyman)

## v0.9.8

- Update PureScript to `0.15.10` (@JordanMartinez)

## v0.9.7

- Update Pursuit version in `pursuit.cabal` (@JordanMartinez)

  The previous release still indicates `v0.9.4`

## v0.9.6

- Update `purescript` to `0.15.8` (support dark theme) (@JordanMartinez)
- Update documentation uploading instructions (@JordanMartinez)
- Bump CI's Ubuntu version to `latest` (@JordanMartinez)

## v0.9.5

Due to an Ubuntu brownout, CI did not build this release properly.

## v0.9.4

- Fix license generation (@JordanMartinez)

- Update `purescript` to `0.15.6` (@JordanMartinez)

## v0.9.3

- Fix license generation (@andys8)

- Bump nginx max body limit to 10m (@thomashoneyman)

- Add cors headers (@sigma-andex)

- Update `purescript` to `0.15.4` (@sigma-andex)
  
  Update to GHC 9.2.3 required by Purescript v0.15.4

## v0.9.2

- Update `purescript` to `0.15.2` (@JordanMartinez)

  This version of the compiler corrects and cleans up
  some of the docs for the builtin `Prim` module.

## v0.9.1

- Fix typo in `ncu` command on `authors.md` page (@kRITZCREEK)

## v0.9.0

- Update `purescript` to `0.15.0` (@JordanMartinez)
- Fix publishing instructions (@JordanMartinez)

## v0.8.7

- Update `purescript` to `0.14.5` and `purescript-cst` to `0.4.0.0` (@JordanMartinez)

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
