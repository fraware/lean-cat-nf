# User guide

This guide is for people **calling CatNF from Lean metaprogramming** (tactics, commands, or `MetaM` utilities). It complements the [README](../README.md) API summary and [What counts as canonical?](WhatCountsAsCanonical.md).

## Which entry point should I use?

| Goal | Suggested API | Notes |
|------|---------------|--------|
| Normalize an `Expr` with the **default** pipeline (flatten segments, rebuild) | `CatNF.Core.normalizeGoalM` | Validates `Config`, rejects metavariable goals. Does **not** run associativity, functor–whisker, or iso passes by itself. |
| Flatten + **associativity / units / tensor spine** on segments, then rebuild | `CatNF.Tactic.normalizeWithProgress` (or call `flattenCompositionM`, then `normalizeAssocUnit`, then `rebuildExpressionM` yourself) | See `CatNF.Tactic` and `CatNF.AssocUnit`. |
| Compare expressions up to associativity | `CatNF.AssocUnit.areAssociativelyEquivalent` | Uses `normalizeAssocUnit` on flattened segments. |
| Compare expressions up to functor–whisker normalization | `CatNF.FunctorWhisker.areFunctoriallyEquivalent` | Uses `normalizeFunctorWhisker` on flattened segments. |
| Cancel adjacent iso `hom`/`inv` on a segment list | `CatNF.Core.shuntIsomorphisms` | Not part of `normalizeGoalM` today. |

If you need **validated** configuration, use `CatNF.Core.createConfig` in `CatNFM`; it runs `validateConfig` before returning. A bare `Config` literal is fine only if you stay within the bounds in the next section.

## Configuration bounds

`validateConfig` rejects values outside these ranges (messages are prefixed with `CatNF Validation Error:` when surfaced through `runCatNFM!` / `normalizeGoalM`):

| Field | Requirement |
|-------|-------------|
| `maxSteps` | `1 … 10000` |
| `timeoutMs` | `1 … 30000` (milliseconds) |
| `simpSet` | If `some s`, then `s` non-empty and length `≤ 100` |
| `maxWorkers` | `1 … 32` |
| `cacheSize` | `1 … 1000000` |
| `maxMemoryBytes` | `1 … 1000000000` (1 GB cap) |

Other `Config` fields (`monoidal`, `trace`, caching, parallelism, etc.) are validated but **not all are consumed** by the minimal `normalizeGoal` path; see [WhatCountsAsCanonical.md](WhatCountsAsCanonical.md).

## Working in `MetaM` versus `CatNFM`

- **`normalizeGoalM`** runs the `CatNFM` pipeline and turns a `CatNFError` into a **Lean exception** (message includes the `CatNF … Error:` prefix from `ToString CatNFError`).
- For **`Except`-style** handling, use `normalizeGoal` in `CatNFM` and `m.run`, or `runCatNFM!` when you want to fail fast on error.

## Expressions that are rejected or fragile

**Metavariables**

- `normalizeGoalM` refuses a **goal** that is an `MVar` (`cannot normalize metavariable goals`).
- **Flattening** refuses a top-level metavariable (`cannot flatten metavariable expressions`).
- Validating `ExprSegment` values rejects metavariables inside iso, functor, whisker, tensor, associator, unitor, braid, and `raw` segments (see `validateExprSegment` in `CatNF.Core`).

**Depth and size**

- Flattening uses a recursion depth limit (**100**); exceeding it raises a normalization error (`expression too deeply nested`).
- Between depth **50** and **100**, flattening can report a **timeout-style** error keyed off `config.timeoutMs` (the message text mentions timing; this is a depth guard, not a wall-clock timer on the whole pipeline).
- Many list-based utilities cap at **1000** segments (`too many segments to process`).

**Empty or degenerate lists**

- Rebuilding requires a non-empty segment list.
- Erasing identities can fail if **every** segment is an identity (`all segments were identities`).

## `raw` segments

If `flattenComposition` does not match a known Mathlib head symbol (`CategoryStruct.comp`, `Functor.map`, `MonoidalCategory.tensorObj`, etc.), the subtree becomes a single **`ExprSegment.raw`** node. The normalizer then mostly **preserves** that subtree as an opaque chunk inside the rebuilt composition.

To get structured segments, the morphism expression should use the **same constants** CatNF pattern-matches on (as in Mathlib’s category-theory library). Custom notation that elaborates to different `Expr` shapes may end up entirely or partly `raw`.

## Learning from the test suite

Concrete `mkApp` / `mkConst` examples and expectations live under `src/CatNF/Tests/`:

| Area | Path |
|------|------|
| Core flattening, rebuild, iso shunting, assoc unit, functor–whisker | `src/CatNF/Tests/Unit/Core.lean` |
| Tactic helpers | `src/CatNF/Tests/Unit/Tactic.lean` |
| Monoidal | `src/CatNF/Tests/Unit/Monoidal.lean` |
| Small end-to-end smoke | `src/CatNF/Tests/Integration/EndToEnd.lean` |

The integration test builds a composition with `CategoryTheory.CategoryStruct.comp` and runs `normalizeGoalM`—a minimal pattern to copy.

## Troubleshooting quick reference

| Message (fragment) | Likely cause |
|--------------------|----------------|
| `cannot normalize metavariable goals` | Pass a concrete `Expr`, not the goal `MVar`. |
| `cannot flatten metavariable expressions` | Subexpression is still an `MVar`. |
| `maxSteps must be greater than 0` / `timeoutMs must be greater than 0` | Adjust `Config` or use `createConfig`. |
| `expression too deeply nested` | Simplify or split the term; depth `> 100` while flattening. |
| `expression segment list too long` | Extremely long composition chain; architectural limit. |
| `CatNF Timeout Error` during flatten | Depth between 50 and 100 hit the flatten guard (see **Depth and size**). |

For version and dependency mismatches, see [Compatibility.md](Compatibility.md). For build and CI issues, see [CONTRIBUTING.md](../CONTRIBUTING.md).

## See also

- [README.md](../README.md) — install, `Config` example, error type listing.
- [WhatCountsAsCanonical.md](WhatCountsAsCanonical.md) — design targets vs wired passes.
- [Compatibility.md](Compatibility.md) — Lean / Mathlib pins.
