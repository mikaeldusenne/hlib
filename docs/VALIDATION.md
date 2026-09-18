# Validation record

Maintenance review: 2026-09-18, starting at `a116d55`.

## Checks completed during preparation

- `cabal check`: no errors or warnings (Cabal 3.18.1.0).
- `cabal sdist`: source archive generated successfully.
- `bash -n scripts/check.sh`: passed.
- YAML syntax parsing: CI, Dependabot, Stack configuration and snapshot lock passed.
- Stack 3.11.1 resolves LTS 24.59 with GHC 9.10.3 and the declared bounds;
  `stack.yaml.lock` records the resolved snapshot hash.
- Executed diagnostics confirm remaining issues: quoted-printable U+0080 gives
  `=02`, U+0800 gives `=E0=80`, DEL is not escaped, existing CRLF gains an encoded
  CR; `simplify (Fraction 0 0)` gives `0`, and `prettyBytes 1` gives `1 kB`.
- Dimensions of all five historical F tables: 34 rows × 19 columns.

- Local execution of the list, statistics/matrix and CSV/HTML/tree test subsets
  passes on GHC 9.10.3, including all four QuickCheck properties (200 cases each).

## Build and test status

Full GHC 9.10.3 compilation is in progress in an isolated local toolchain on
Arch Linux x86_64. This record will be updated with the results before the work
is handed over. GHC 9.6.7, 9.12.4 and 9.14.1 and a full Stack build are configured
as CI jobs; configuration and dependency resolution alone do not establish that
those jobs pass.

## Limits of validation

The suite exercises selected regressions and representative public behaviour;
it does not prove every legacy export correct. There is no live HTTP test,
interactive viewer test, R comparison of statistical routines, exhaustive CSV/
XLSX corpus, Windows/macOS validation or independent audit of historical F-table
values. Existing compiler warnings are recorded as maintenance debt rather than
hidden or treated as successful API validation. See [REVIEW.md](REVIEW.md).
