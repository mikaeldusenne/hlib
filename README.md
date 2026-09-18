# hlib

A personal Haskell toolbox for lists, fractions, statistics, CSV/XLSX data,
small renderers and Unix helpers. This is experimental code, with an intentionally
preserved legacy API. It is useful for personal scripts; it is not a validated
statistics package or a general-purpose parsing framework.

The 2026 maintenance work keeps the 23 module names and existing entry points.
It updates the build, adds regression tests and documentation, and fixes selected
bugs. See [the full review (French)](docs/REVIEW.md) for the remaining problems,
[the API guide](docs/API.md) for contracts, and [the changelog](ChangeLog.md) for
behaviour changes.

## Build and test

Use GHCup to select a compiler independently of your distribution's package
updates. The reference Stack snapshot is **LTS 24.59 / GHC 9.10.3**. Cabal CI
also checks GHC 9.6.7, 9.12.4 and 9.14.1. Those are CI targets, not a claim that
every function has been validated on every platform.

After installing [GHCup](https://www.haskell.org/ghcup/):

```sh
ghcup install ghc 9.10.3
ghcup set ghc 9.10.3
ghcup install cabal 3.18.1.0
ghcup set cabal 3.18.1.0

git clone https://github.com/mikaeldusenne/hlib.git
cd hlib
cabal update
cabal build all --enable-tests
cabal test all --test-show-details=direct
cabal exec -- runghc examples/Quickstart.hs
```

Cabal downloads Haskell dependencies from Hackage. The first build includes
XLSX, image and TLS dependencies, so it takes longer than this small codebase
might suggest. There is no application binary to install or service to deploy.

On Arch Linux, the native prerequisites can be installed with:

```sh
sudo pacman -Syu --needed base-devel curl git gmp libffi ncurses zlib zstd expat bzip2
```

Then use the same GHCup instructions. Keep GHCup's `~/.ghcup/bin` ahead of a
system GHC when using this workflow; check `command -v ghc` and `ghc --version`
if Cabal or your editor selects a different compiler. HLS is optional and must
support the GHC version you select. Do not use `sudo cabal` or `sudo stack`.

Alternatively, with Stack installed:

```sh
stack test
stack haddock
stack ghci
```

`stack.yaml` selects the compiler and dependency snapshot; both tools read
`hlib.cabal`. You do not need Hpack. Do not alternate tools in the middle of a
build and expect their caches or dependency plans to be shared.

## Use it in another project

For local development, list both packages in your application's `cabal.project`:

```cabal
packages: . ../hlib
```

Add `hlib` to that application's `build-depends`. For a Git dependency, use
`source-repository-package` with an actual full commit SHA; see
[distribution instructions](docs/RELEASING.md). The package version in this
checkout does not imply that it exists on Hackage.

Use qualified imports because names such as `List`, `Matrix` and `Json` are
deliberately retained and can collide with other packages:

```haskell
import qualified CSV
import qualified List
import qualified Maths
import qualified Stats

-- Rows, including the header:
CSV.parseCSV ',' "name,score\nAlice,10\nBob,14\n"
-- [["name","score"],["Alice","10"],["Bob","14"]]

List.safe_nth 2 [10, 14 :: Int]       -- Just 14 (one-based)
Stats.median [10, 14 :: Double]      -- 12.0
Stats.choose 5 2 :: Integer          -- 10
Maths.readFraction "1.25" + Maths.readFraction "3/4"  -- 2
```

These expressions can be tried in `cabal repl`. The complete runnable version
is [examples/Quickstart.hs](examples/Quickstart.hs).

## Find your way around

| Area | Modules |
| --- | --- |
| Lists, strings and small helpers | `List`, `Tuple`, `Misc`, `Paths`, `Trees` |
| Numbers and statistics | `Maths`, `Constants`, `Stats`, `Distribution_Tables`, `Kmeans` |
| Structured data | `CSV`, `DF`, `Matrix`, `Json` |
| Rendering and encodings | `Html`, `SVG_creator`, `Colors`, `Pixels`, `Bases`, `QuotedPrintable`, `Smiley` |
| Effects | `Hunix`, `Requester` |

Generate the API reference with
`cabal haddock all --disable-documentation --haddock-hyperlink-source`.
Despite its name, `--disable-documentation` here skips dependency documentation;
Haddock still documents the selected local package.
Cabal prints the generated HTML location under `dist-newstyle`. Each module has
an overview; documentation of individual legacy functions is still incomplete.
The handwritten [API guide](docs/API.md) explains the main entry points and traps.

Some convenience functions still require personal files or external programs:
`Smiley.smileyList*`, `List.dico`, `Matrix.loadMatrix`, `Hunix.uploadFile`,
`Kmeans.runTest` (`feh`), and `Stats.table_qf_from_R_lol` (`Rscript`). They are
not needed for the normal build or test suite. JSON has safe `Maybe` helpers;
several older parsers and numerical routines can still throw on invalid input.

See [CONTRIBUTING.md](CONTRIBUTING.md) for development checks. The existing
BSD-3-Clause license text is in [LICENSE](LICENSE); its inherited placeholder
attribution needs confirmation before a public package release.
