# Distribution and releases

This repository contains a source library. Deployment means selecting a source
revision, verifying the package archive and optionally publishing it. It does not
need a Docker image, systemd unit or a globally registered library.

## Consume a pinned Git revision

In a downstream application's `cabal.project`:

```cabal
packages: .

source-repository-package
  type: git
  location: https://github.com/mikaeldusenne/hlib.git
  tag: REPLACE_WITH_THE_FULL_COMMIT_SHA_YOU_HAVE_REVIEWED
```

Replace the placeholder with a real commit from the branch/tag you intend to
consume, then add `hlib` to the application's `build-depends`. Cabal needs Git
and network access for the initial checkout. Freeze the **application's** plan
with `cabal freeze` if reproducibility is needed. This library intentionally does
not impose one Cabal solver plan on all consumers.

Stack consumers can add the same repository and commit under `extra-deps`:

```yaml
extra-deps:
- git: https://github.com/mikaeldusenne/hlib.git
  commit: REPLACE_WITH_THE_FULL_COMMIT_SHA_YOU_HAVE_REVIEWED
```

The downstream resolver must satisfy `hlib.cabal`'s dependency bounds. For this
repository, `stack.yaml` pins LTS 24.59. Keep its generated `stack.yaml.lock`
versioned when the snapshot is updated. The CI tests both build tools; a successful
Cabal build alone does not validate the Stack snapshot.

## Prepare an archive

1. Work on a dedicated branch and update `ChangeLog.md` and the package version.
2. Run `bash scripts/check.sh`, and inspect all compiler jobs in GitHub Actions.
3. Run `cabal sdist`; the archive is under `dist-newstyle/sdist/`.
4. Inspect its contents with `tar -tzf dist-newstyle/sdist/hlib-0.2.0.0.tar.gz`.
5. Review `docs/REVIEW.md` and the API changes with the maintainer before release.

The check script builds and tests an extraction in a new temporary directory,
so a local file omitted from the archive cannot hide a packaging fault.
Generated Haddock is also available as a CI artifact alongside the source archive.
There is deliberately no upload on a tag or push and no release credential in CI.

The package's next version is 0.2.0.0, rather than a patch release: compiler and
dependency support changed, HTML escaping changes output, and fixes can change
results that old consumers relied on. Follow the
[Haskell Package Versioning Policy](https://pvp.haskell.org/); keeping old names
alone does not guarantee compatibility. No `0.2.0.0` release/tag has been implied
by changing the version field.

## Before a public Hackage release

The inherited `LICENSE` contains `Author name here`. Its BSD-3-Clause terms have
been kept unchanged; the attribution must be confirmed by the owner before
publishing. Also verify the package name is available/owned on Hackage, select
an appropriate maintainer contact and decide which experimental entry points
you actually want to support. `cabal check` cannot establish any of these facts.

After those decisions and successful validation, a maintainer can use:

```sh
cabal upload dist-newstyle/sdist/hlib-0.2.0.0.tar.gz
```

This creates a Hackage **candidate** for inspection (without `--publish`). Review
its rendered package page and documentation before making any public release.
The final publication, tag and release are separate maintainer actions.

An AUR package is optional future work. It is unlikely to help more than a pinned
source dependency for this personal library: a Haskell binary package is tied to
its GHC/package ABI and would require ongoing rebuilds. Start with reliable source
archives; add distribution-specific packaging only when a consumer needs it.
