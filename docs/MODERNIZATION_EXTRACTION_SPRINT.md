# Modernization and extraction sprint

This document records the first modernization and extraction plan for `lean-cat-nf` as part of the broader category-theory contribution program targeting Mathlib and CSLib.

## Current repository position

`lean-cat-nf` is a category-theory normalization engine for Lean expressions. It targets composition, identities, functorial maps, whiskering, isomorphism transport, monoidal structure, and related morphism-normalization patterns.

The repository is valuable for the contribution program primarily as an internal proof-friction detector. It can reveal repeated categorical rewrite patterns that should become Mathlib simp lemmas, reassociation lemmas, extensionality lemmas, or documentation examples.

Current constraints (post-sprint):

- Toolchain in `lean-toolchain`: `leanprover/lean4:v4.31.0-rc1`.
- `Lakefile.lean` pins Mathlib at `v4.31.0-rc1`.
- The README states that no `cat_nf` tactic syntax is shipped yet and that users should call metaprogramming entrypoints such as `normalizeGoalM` or `catNFImpl`.
- The core file imports Mathlib category theory, monoidal category infrastructure, whiskering, Lean metaprogramming modules, and tactic modules.
- The current `Config` type includes resource, parallelism, caching, tracing, and memory-budget fields, which are useful for a standalone tool but too broad for a first Mathlib contribution.

## Sprint objective

The objective is to modernize the repository against the Lean 4.31 / current-Mathlib line and extract upstreamable mathematical proof infrastructure from the normalization cases.

The first upstream outputs should be Mathlib lemmas and examples, not a Mathlib tactic. A tactic PR should wait until the kernel is stable on current Mathlib and has a focused test suite over real category-theory goals.

## Modernization gates

### Gate 1: port the repository to the current Mathlib line

Update the Lean and Mathlib pins to the current Mathlib baseline and run the full suite.

Required commands:

```bash
lake update
lake build
lake exe test-runner
lake exe test-runner-final
lake exe bench
```

Expected first failures to check:

- moved Mathlib imports for category theory, monoidal categories, and whiskering;
- Lean metaprogramming API drift since Lean 4.8;
- changes in `Expr`, `MetaM`, tactic elaboration, and exception handling;
- old references to `Mathlib.Tactic.Basic` or tactic namespaces;
- monoidal category notation and coherence theorem renames.

### Gate 2: reduce core imports

The normalization kernel should be split into a small dependency core and optional categorical frontends.

Recommended target layout:

```text
src/CatNF/Core/Error.lean
src/CatNF/Core/Config.lean
src/CatNF/Core/Segments.lean
src/CatNF/Core/Normalize.lean
src/CatNF/Category/Basic.lean
src/CatNF/Category/FunctorWhisker.lean
src/CatNF/Category/IsoTransport.lean
src/CatNF/Monoidal/Core.lean
src/CatNF/Tactic.lean
```

The core should not import all category-theory and monoidal files at once.

### Gate 3: build a lemma-extraction ledger

For every successful normalization case, record the corresponding Mathlib candidate.

Required ledger columns:

- input goal shape;
- normalization step;
- existing Mathlib theorem used;
- missing theorem, if any;
- proposed theorem statement;
- likely file path in Mathlib;
- review risk.

This ledger is the main product of the sprint.

## Extraction targets

### Target A: category rewrite lemmas

Likely Mathlib candidates:

- functor-map identity and composition variants used under categorical composition;
- naturality variants that work well with `simp` and `reassoc`;
- whiskering simplification lemmas;
- isomorphism hom/inv cancellation patterns;
- transport lemmas that reduce boilerplate in equivalence and isomorphism proofs.

### Target B: documentation examples

Many `CatNF` successes should become documentation examples before becoming tactic infrastructure.

Candidate examples:

- normalizing a functoriality goal;
- normalizing a naturality square;
- normalizing an isomorphism cancellation proof;
- normalizing a whiskered natural transformation proof.

### Target C: external tactic hardening

Keep the tactic external until it satisfies all of the following conditions:

- compiles on the current Mathlib line;
- has tests over current Mathlib category-theory files;
- has predictable failure messages;
- does not depend on broad imports in the public API;
- has a narrow initial scope.

## Non-upstream material for now

The following should remain repository-local during this sprint:

- caching and parallel-processing infrastructure;
- memory-budget configuration;
- production benchmark tooling;
- broad monoidal coherence support;
- any tactic syntax proposal for Mathlib;
- Docker and one-line install material.

## First PR candidates generated from this repo

1. Local modernization PR: port the repository to Lean 4.31 and current Mathlib.
2. Local architecture PR: split the normalization core from category and monoidal frontends.
3. Local audit PR: add an extraction ledger of normalization cases and missing Mathlib lemmas.
4. Mathlib candidate PR: add small naturality or whiskering simp lemmas discovered by the ledger.
5. Mathlib candidate PR: add documentation examples for standard categorical normalization patterns.

## Build certification status

Gate 1 certified on **2026-06-09** (Windows local + updated `.github/workflows/ci.yml` for `v4.31.0-rc1`):

| Command | Status |
|---------|--------|
| `lake update` | Pass |
| `lake build` | Pass |
| `lake build CatNFTests` | Pass |
| `lake exe test-runner` | Pass (11/11) |
| `lake exe test-runner-final` | Pass |
| `lake exe bench` | Pass |

Gate 2 complete: kernel under `src/CatNF/Core/` (Mathlib-free), category frontends under `src/CatNF/Category/`, pipeline in `CatNF.Category.Pipeline`.

Gate 3 complete: [`EXTRACTION_LEDGER.md`](EXTRACTION_LEDGER.md) with 12+ concrete lemma cases.
