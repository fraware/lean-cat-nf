# Compatibility

This document describes how **lean-cat-nf** tracks **Lean 4** and **Mathlib4**, how to depend on the package from another Lake project, and what to record when reporting issues.

## Pinned versions (source of truth)

| Component | Source | Current pin |
|-----------|--------|-------------|
| Lean 4 | [`lean-toolchain`](../lean-toolchain) | `leanprover/lean4:v4.8.0` |
| Mathlib4 (requested revision) | [`Lakefile.lean`](../Lakefile.lean) `require mathlib` | `v4.8.0` |
| Mathlib4 (exact Git revision) | [`lake-manifest.json`](../lake-manifest.json) | The `rev` field on the `mathlib` package entry (locked when you last ran `lake update`) |

After a bump, **both** `Lakefile.lean` and `lake-manifest.json` should be consistent: the manifest records the concrete commit Lake resolved for the tag or branch you requested.

**Consumers**: For reproducible builds, pin **this repository** with a commit SHA (and optionally record the manifest revision of Mathlib you tested against). Pinning only `main` can move Lean/Mathlib alignment without warning.

## Version tags

Semantic version tags may be added for releases. Until you rely on a published tag, prefer a **Git revision** in your `lakefile.lean`.

## Depending on lean-cat-nf (Lake)

The package name in this repository is `«lean-cat-nf»`. Minimal example:

```lean
require «lean-cat-nf» from git
  "https://github.com/fraware/lean-cat-nf.git" @ "main"
```

For reproducibility, use a tag or commit:

```lean
require «lean-cat-nf» from git
  "https://github.com/fraware/lean-cat-nf.git" @ "abc1234deadbeef..."
```

### Imports

Typical modules (see [README.md](../README.md) for API details):

- `CatNF.Core` — `normalizeGoal`, `normalizeGoalM`, `Config`, `flattenComposition`
- `CatNF.Tactic` — `catNFImpl`, helpers such as `normalizeWithProgress`
- `CatNF.RewriteRules`, `CatNF.Attr` — rule registration and validation

### Lean and Mathlib alignment

Your project’s `lean-toolchain` should match a Lean version **compatible with the Mathlib revision** that **lean-cat-nf** uses. Mismatches usually surface as `lake build` failures in Mathlib or CatNF imports. When in doubt, align your toolchain and Mathlib tag with the versions listed at the top of this file.

## Mathlib / Lean bump (contributors)

1. Pick a Mathlib release tag that matches the target Lean toolchain ([mathlib4 releases](https://github.com/leanprover-community/mathlib4/releases)).
2. Update [`lean-toolchain`](../lean-toolchain).
3. Point `require mathlib` in [`Lakefile.lean`](../Lakefile.lean) at the same tag (or a known-good commit).
4. Run `lake update` and `lake build`; fix breakages under `src/CatNF/` and tests.
5. Run `lake exe test-runner` before opening a PR.

More detail: [CONTRIBUTING.md](../CONTRIBUTING.md).

## Verifying a local setup

```bash
lake exe cache get
lake build
lake exe test-runner
```

On slow or fresh machines, allow time for Mathlib and dependencies to build after the first `lake build`.

### Windows

If linking the test executable fails due to command-line length limits, see [CONTRIBUTING.md](../CONTRIBUTING.md). CI still exercises a compile check on Windows.

### Docker

Prebuilt images (for example `ghcr.io/fraware/lean-cat-nf`) may lag the `main` branch slightly. For bit-for-bit parity with source pins, build from the repository at a known commit.

## Reporting issues

Include:

- Lean version (contents of `lean-toolchain`)
- Mathlib pin: `inputRev` from `Lakefile.lean` **and** the locked `rev` for `mathlib` in `lake-manifest.json`
- **lean-cat-nf** revision (commit or tag)
- Minimal repro: small Lean snippet or failing test name, plus `lake build` / `lake exe test-runner` output

## Policy (informal)

- **Main branch** is expected to build with the Lean and Mathlib versions recorded in this repository.
- Breaking API changes should be mentioned in commit messages and release notes when using version tags.
- There is no shipped `cat_nf` user-facing tactic syntax yet; use the metaprogramming entry points in [README.md](../README.md) and the notes in [WhatCountsAsCanonical.md](WhatCountsAsCanonical.md) about which passes are wired where.
