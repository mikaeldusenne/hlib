# Validation record

Maintenance review: 2026-09-18, starting at `a116d55`.

## Native Arch Linux validation

`bash scripts/check.sh` passed on Arch Linux x86_64 with GHC 9.10.3 and
Cabal 3.14.2.0, using an isolated GHCup toolchain. The system compiler and shell
configuration were not changed. The script completed all of these checks:

- `cabal check`: no package errors or warnings.
- Compilation of all 23 library modules and the test suite.
- 45 example/regression assertions and four QuickCheck properties, 200 generated
  cases per property (800 cases per suite execution).
- Haddock HTML and linked sources for the library.
- Execution of `examples/Quickstart.hs` with the documented output.
- Creation and extraction of the source archive, followed by another package
  check, independent compilation, complete test run and quickstart execution.

The local plan includes Aeson 2.2.5.1, XLSX 1.2.0, lens 5.3.6 and QuickCheck 2.16.
Cabal 3.18.1.0 also passes `cabal check`; the full native build used 3.14.2.0.

## CI validation

All five jobs passed on Ubuntu 24.04 at code revision `0b43ec7`:

| Tool | Compiler | Result |
| --- | --- | --- |
| Cabal 3.18.1.0 | GHC 9.6.7 | Complete check script passed |
| Cabal 3.18.1.0 | GHC 9.10.3 | Complete check script passed; source/Haddock artifact uploaded |
| Cabal 3.18.1.0 | GHC 9.12.4 | Complete check script passed |
| Cabal 3.18.1.0 | GHC 9.14.1 | Complete check script passed |
| Stack 3.11.1 / LTS 24.59 | GHC 9.10.3 | Compilation and complete test suite passed |

[Successful workflow and artifacts](https://github.com/mikaeldusenne/hlib/actions/runs/35317155187).
The Stack job also exercises XLSX 1.1.4, while the native Cabal plan uses 1.2.0.

## Additional checks

- `bash -n scripts/check.sh`, YAML syntax parsing and local documentation links.
- Stack snapshot resolution and the committed `stack.yaml.lock` hash.
- Executed diagnostics confirm remaining issues: quoted-printable U+0080 gives
  `=02`, U+0800 gives `=E0=80`, DEL is not escaped, existing CRLF gains an encoded
  CR; `simplify (Fraction 0 0)` gives `0`, and `prettyBytes 1` gives `1 kB`.
- Dimensions of all five historical F tables: 34 rows × 19 columns.

## Limits of validation

The suite exercises selected regressions and representative public behaviour;
it does not prove every legacy export correct. There is no live HTTP test,
interactive viewer test, R comparison of statistical routines, exhaustive CSV/
XLSX corpus, Windows/macOS validation or independent audit of historical F-table
values. Existing compiler and Haddock warnings remain maintenance debt; passing
package checks do not mean the source is warning-free. Documentation coverage of
individual legacy exports remains incomplete. See [REVIEW.md](REVIEW.md).
