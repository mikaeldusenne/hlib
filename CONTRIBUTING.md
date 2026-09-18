# Contributing

`hlib.cabal` is the package description for both Cabal and Stack. Do not
reintroduce a generated `package.yaml` alongside it. Keep the exposed modules
and the names used by existing callers unless an API change is intentional.

Use a dedicated branch. For a normal development cycle:

```sh
cabal update
cabal build all --enable-tests
cabal test all --test-show-details=direct
```

Before proposing a release, run `bash scripts/check.sh`. This also builds
Haddock, runs the quickstart and builds/tests a fresh extraction of the source
archive. Tests do not contact a server, open a viewer or require personal data.
The shell script requires Bash, tar and mktemp; the library does not.

For changes to behaviour, add a small regression test that demonstrates the
fault, and document the compatibility impact in `ChangeLog.md`. QuickCheck
properties exercise chunking, sorting, binomial coefficients and base conversion.
Use `Integer` when a combinatorial result may overflow `Int`.

New public functions should have a type signature and a Haddock comment, with
their indexing, units, empty-input behaviour and failure conditions explained.
Prefer `Maybe` or `Either` for recoverable failures; keep legacy partial entry
points as wrappers when that avoids breaking consumers. Prefer qualified imports
for the generic module names (`List`, `Json`, `Matrix`, etc.).

Warnings are enabled, but are not errors: the inherited API has substantial
missing-signature, partial-pattern and defaulting debt. Fix warnings in the code
you touch when the fix does not accidentally change an inferred public type.
Do not silence all warnings or reformat every module in a functional change.
Haskell2010 is explicit to preserve this code's inference behaviour. GHC2024
can be considered for new modules separately, after compiling and testing them.

No formatting tool is required. HLS, HLint and Fourmolu are useful optional
tools; inspect suggestions before applying them to this unusually named API.
Do not rename Unicode identifiers or apostrophe-separated names mechanically.

See [the review](docs/REVIEW.md) for the prioritised backlog and
[release instructions](docs/RELEASING.md) for distribution.
