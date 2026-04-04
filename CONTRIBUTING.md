# Contributing to CatNF

## Prerequisites

- [elan](https://github.com/leanprover/elan) with Lean matching [`lean-toolchain`](lean-toolchain) (currently **v4.8.0**, same major.minor as Mathlib in [`Lakefile.lean`](Lakefile.lean)).
- Git

## Build and test

```bash
lake exe cache get   # recommended before first Mathlib build
lake build
lake exe test-runner
```

`lake test` runs the same test executable as `lake exe test-runner` (see [`Lakefile.lean`](Lakefile.lean)).

Optional:

```bash
lake exe bench
lake exe test-runner-final
```

## Documentation

Upstream [`doc-gen4`](https://github.com/leanprover/doc-gen4) may not publish a tag that matches every Lean version. For **Lean 4.8.0** you can either move the project to a Lean pair that has a matching doc tool, or use a separate small Lake project that depends on this package and a compatible doc revision.

The **docs** job in continuous integration is turned off until that setup is pinned.

Overview docs: [README.md](README.md) and [`docs/`](docs/). The repo includes [`.editorconfig`](.editorconfig) for basic editor consistency.

## Production-oriented tests

Older layouts used a separate `tests/Production/` folder. Comparable checks now live under [`src/CatNF/Tests/Performance/ProductionOptimizations.lean`](src/CatNF/Tests/Performance/ProductionOptimizations.lean).

## Windows

Linking `lake exe test-runner` or `lake exe bench` can fail with **error 87** when the linker command line is too long. Linux CI does not hit this. Locally you can use a shorter project path, WSL, or Docker. To still typecheck tests without building the standalone test executable, run:

```bash
lake build +CatNF.Tests.TestRunner
```

## Mathlib / Lean upgrade

1. Pick a Mathlib release that matches the Lean version you want ([mathlib releases](https://github.com/leanprover-community/mathlib4/releases)).
2. Update [`lean-toolchain`](lean-toolchain).
3. Point `require mathlib` in [`Lakefile.lean`](Lakefile.lean) at that tag.
4. Run `lake update` and `lake build`.
5. Fix breakages under `src/CatNF/` and in tests.
6. Run `lake exe test-runner` before opening a pull request.

Keep CI on a single primary Lean version that matches the Mathlib pin.

## Optional CI settings

Some forks add an API key for an external license-reporting service; if it is missing, that step is skipped. No other secrets are required for the default build and test jobs.

## Style

- Follow common Mathlib and Lean naming and layout.
- Keep changes focused on the issue you are solving.
- In CatNF sources, avoid naming a loop variable `rewrite` (it can clash with Lean’s `Meta.rewrite`).

## Pull requests

- Say what changed and why.
- Call out intentional changes to normalization or benchmark numbers.
- Update [README.md](README.md) or [docs/](docs/) when behavior or install steps change for users.
