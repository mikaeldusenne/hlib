# Changelog for hlib

## 0.2.0.0 — unreleased

### Project and documentation

- Make `hlib.cabal` the single, versioned package description for Cabal and Stack;
  remove Hpack configuration, the unused executable stub and redundant Setup.hs.
- Replace Stack LTS 10.7 with LTS 24.59 (GHC 9.10.3); define dependency bounds for
  modern compilers and Aeson 2; remove unused direct dependencies.
- Enable warnings, regression tests and QuickCheck properties. Add CI for GHC
  9.6.7, 9.10.3, 9.12.4 and 9.14.1, and a separate Stack check.
- Validate source archives after extraction; produce documentation/source CI
  artifacts without automatically publishing them.
- Add build, usage, contributor and release guides, module Haddock overviews,
  a runnable example and a complete review with an explicit backlog.

### Behaviour changes

- `safe_nth` returns `Nothing` past the end instead of looping. It stays one-based.
- `reduce` accepts singleton lists; `flatten` accepts empty/singleton outer lists.
- `transpose` now has `Data.List.transpose` semantics on empty and ragged input.
- `splitEach` rejects non-positive sizes; `splitWhen` rejects an empty separator;
  `replaceStr` treats an empty pattern as a no-op instead of looping.
- Correct odd/even medians and binomial-coefficient edge cases; reject negative
  population sizes and return zero for out-of-range choices.
- `Matrix.rowN` uses the column count to extract rows of rectangular matrices.
- Adapt JSON object lookup to Aeson 2 and add `lookupMaybe`/`unStringMaybe`.
- Implement HTTP `GET` construction and add `sendWith` for manager reuse. The
  existing `send` response encoding remains Char8.
- HTML text and attribute values are escaped. Existing callers supplying raw
  markup/entities through `Text` will see escaped output and must adapt.
- K-means waits for every centre to settle, retains empty-cluster centres and
  rejects invalid counts, non-finite coordinates and ragged/empty points.

Existing module names and entry points are retained. Minimum compiler support is
now GHC 9.6 (`base >= 4.18`); the old Stack/GHC 8.2 environment is no longer a target.
The numerical/parser limitations described in `docs/REVIEW.md` remain; this release
is not a certification of the experimental algorithms.

## 0.1.0.0

Original personal library, imported in 2018.
