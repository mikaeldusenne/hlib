#!/usr/bin/env bash
# Run from anywhere; use the caller's GHC and Cabal installations.
set -euo pipefail
cd "$(dirname "$0")/.."

cabal check
cabal build all --enable-tests
cabal test all --enable-tests --test-show-details=direct
cabal haddock all --haddock-hyperlink-source
cabal exec -- runghc examples/Quickstart.hs

# Building the checkout alone does not catch missing files in a release.
hlib_tmp=$(mktemp -d)
trap 'rm -rf -- "$hlib_tmp"' EXIT
cabal sdist --output-directory="$hlib_tmp"
shopt -s nullglob
hlib_archives=("$hlib_tmp"/hlib-*.tar.gz)
if (( ${#hlib_archives[@]} != 1 )); then
  echo 'Expected exactly one hlib source archive' >&2
  exit 1
fi
mkdir "$hlib_tmp/unpacked"
tar -xzf "${hlib_archives[0]}" -C "$hlib_tmp/unpacked"
hlib_sources=("$hlib_tmp"/unpacked/hlib-*)
if (( ${#hlib_sources[@]} != 1 )) || [[ ! -d "${hlib_sources[0]}" ]]; then
  echo 'Expected exactly one extracted hlib source directory' >&2
  exit 1
fi
cd "${hlib_sources[0]}"
cabal check
cabal build all --enable-tests
cabal test all --enable-tests --test-show-details=direct
cabal exec -- runghc examples/Quickstart.hs
