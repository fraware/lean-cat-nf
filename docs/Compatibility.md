# Compatibility

This document describes how **lean-cat-nf** tracks **Lean 4** and **Mathlib4**, and how to depend on the package from another Lake project.

## Pinned versions (source of truth)

| Component | Source | Current pin (as of this doc revision) |
|-----------|--------|----------------------------------------|
| Lean 4 | [`lean-toolchain`](../lean-toolchain) | `leanprover/lean4:v4.8.0` |
| Mathlib4 | [`Lakefile.lean`](../Lakefile.lean) `require mathlib` | tag `v4.8.0` |

When these files change, treat that as the supported pair until the next bump.

## Version tags

Semantic version tags may be added over time for releases. Until you rely on a published tag, pin a **Git revision** (commit SHA) in your `lakefile.lean` for reproducibility.

## Depending on lean-cat-nf (Lake)

The package name in this repository is `«lean-cat-nf»`. Example:

```lean
require «lean-cat-nf» from git
  "https://github.com/fraware/lean-cat-nf.git" @ "main"
```

Replace `"main"` with a tag or revision string as needed:

```lean
require «lean-cat-nf» from git
  "https://github.com/fraware/lean-cat-nf.git" @ "abc1234deadbeef..."
```

Then import modules such as `CatNF.Core`, `CatNF.Tactic`, `CatNF.RewriteRules` (see [README.md](../README.md)).

## Mathlib / Lean bump (contributors)

1. Choose a Mathlib release tag that matches the target Lean (see [mathlib4 releases](https://github.com/leanprover-community/mathlib4/releases)).
2. Update [`lean-toolchain`](../lean-toolchain).
3. Point `require mathlib` in [`Lakefile.lean`](../Lakefile.lean) at the same tag.
4. Run `lake update` and `lake build`; fix breakages in `src/CatNF/` and tests.
5. Run `lake exe test-runner` before opening a PR.

Details: [CONTRIBUTING.md](../CONTRIBUTING.md).

## Verifying a local setup

```bash
lake exe cache get
lake build
lake exe test-runner
```

## Reporting issues

Include:

- Lean version (`lean-toolchain`)
- Mathlib pin (`Lakefile.lean` / `lake-manifest.json`)
- lean-cat-nf revision (commit or tag)
- Minimal repro or failing `lake` / test output

## Policy (informal)

- **Main branch** is expected to build with the pinned Lean and Mathlib versions in the repo.
- Breaking API changes should be called out in commit messages and release notes when you use version tags.
- There is no built-in `cat_nf` tactic yet; use the normalization functions described in [README.md](../README.md).
