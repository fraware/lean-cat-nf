# What counts as canonical?

This document describes **target** normal forms the CatNF design aims for, and how that relates to **what runs in code today**. For calling normalization from metaprogramming, see [README.md](../README.md). Examples are **illustrative**; Mathlib pretty-printing and exact lemma names may differ.

## Design targets versus the main entry point

`CatNF.Core.normalizeGoal` / `normalizeGoalM` currently:

1. Validates `Config` and rejects metavariable goals.
2. **Flattens** the input `Expr` into a list of `ExprSegment` constructors (composition, identities, isomorphisms, functor maps, whiskering, monoidal pieces, or a `raw` fallback).
3. **Rebuilds** a single composed `Expr` from that list (nested `CategoryStruct.comp`, which corresponds to **right-associated** `≫` when pretty-printed with Mathlib’s usual right-associative notation).

It does **not** yet chain the heavier rewrite passes (associativity, units, functor–whisker merging, iso cancellation, monoidal coherence) into that main path. Those passes exist in other modules and in tests or helper entry points; see the table below.

| Intended behavior | Where it lives today | Wired into `normalizeGoalM`? |
|-------------------|----------------------|--------------------------------|
| Right-associated `≫`, drop identities, tensor associators / unitors (segment level) | `CatNF.AssocUnit` (`normalizeAssocUnit`) | No |
| `map` / whisker interaction (`map_comp`-style splits and merges) | `CatNF.FunctorWhisker` (`normalizeFunctorWhisker`) | No |
| Adjacent `hom`/`inv` cancellation on segment lists | `CatNF.Core.shuntIsomorphisms` | No |
| Further iso composition rules | `CatNF.IsoTransport` | No |
| Monoidal coherence rewrites on segments | `CatNF.Monoidal.Coherence`, `CatNF.Monoidal.Core` | No |

`Config` fields such as `monoidal`, caching, and parallelism are validated but **not** fully consumed by the minimal `normalizeGoal` pipeline yet (see comments in `CatNF.Core`). Treat them as **reserved** for upcoming integration unless you compose passes yourself.

**Helpers**: `CatNF.Tactic.normalizeWithProgress` flattens, runs `normalizeAssocUnit`, and rebuilds—useful when you want associativity/unit/tensor segment normalization without relying on `normalizeGoal` alone.

---

## Categories

### Composition

**Target**: Right-associated compositions (same as the shape `rebuildExpression` produces from a flat segment list).

```lean
-- Target shape: f ≫ (g ≫ h)  (often written `f ≫ g ≫ h` with right-associative `≫`)
f ≫ g ≫ h

-- Non-target: left-associated tree (different parenthesization)
(f ≫ g) ≫ h
```

**Rule (design)**: Use associativity so the morphism tree is right-nested. **`normalizeAssocUnit`** implements this on `ExprSegment` trees via `applyAssociativity`.

### Identity morphisms

**Target**: No redundant identity morphisms in the morphism expression.

```lean
-- Target: f
f

-- Non-target
f ≫ 𝟙 Y
𝟙 X ≫ f
```

**Rule (design)**: Remove identities with `id_comp` / `comp_id` (Mathlib). **`normalizeAssocUnit.removeIdentities`** performs analogous cleanup on segment lists.

### Isomorphisms

**Target**: Cancel adjacent `hom ≫ inv` and `inv ≫ hom` when they refer to the same isomorphism, and simplify composed iso legs where the library provides lemmas.

```lean
-- Reducible chain (non-target when cancellation applies)
iso.hom ≫ iso.inv

-- Target: identity on the relevant object (conceptually `𝟙 _`)
```

**Rule (design)**: Use isomorphism laws (`hom_inv_id`, `inv_hom_id`, etc.). Segment-level cancellation is implemented in **`CatNF.Core.shuntIsomorphisms`** / **`CatNF.IsoTransport.normalizeIsoCompositions`**; it is **not** applied inside `normalizeGoalM` today.

### Functor maps

**Target**: A single functor application to a **composed** inner morphism when consecutive maps share the same functor—aligned with the direction of `CategoryTheory.Functor.map_comp` (from `F.map f ≫ F.map g` to `F.map (f ≫ g)`).

```lean
-- Target when F is fixed and functoriality applies
F.map (f ≫ g)

-- Non-target: two maps composed (can be simplified)
F.map f ≫ F.map g
```

**Rule (design)**: Functoriality (`map_comp`). In **`CatNF.FunctorWhisker`**, `flattenMapComp` splits `F.map` over an inner `comp`, and **`applyFunctoriality`** merges adjacent `functor_map` segments for the same `F` back into one `F.map` of a composed argument. The **final** shape after `normalizeFunctorWhisker` favors the combined `F.map (f ≫ g)` form when merging applies.

### Whiskering

**Target**: One whisker around a **composed** morphism when the functor or natural-transformation side matches—consistent with `whiskerLeft_comp` / `whiskerRight_comp`.

```lean
-- Target (left whisker): functor applied to a composition
F ◁ (f ≫ g)

-- Non-target: duplicated whisker then compose (intermediate / expandable form)
(F ◁ f) ≫ (F ◁ g)
```

Right whiskering is analogous: `(f ≫ g) ▷ G` versus `f ▷ G ≫ g ▷ G`.

**Rule (design)**: Whiskering lemmas from Mathlib. **`standardizeWhiskering`** expands `F ◁ (f ≫ g)` into composed whiskers; **`applyFunctoriality`** can merge back when the functor matches.

---

## Monoidal categories

Monoidal, braided, and symmetric behavior depends on the concrete `MonoidalCategory` / `BraidedCategory` instance and on which rewrites are available. The following is the **intended** shape of normal forms on segments, not a guarantee for every morphism type. **`normalizeMonoidal`** in `CatNF.Core` is currently a pass-through; **`CatNF.Monoidal.Coherence.normalizeCoherencePass`** contains segment rewrites used in tests and future wiring.

### Tensor products

**Target**: Right-associated tensors (matching `applyAssociators` / coherence passes that rewrite `tensor (tensor f g) h` to `tensor f (tensor g h)`).

```lean
-- Target: f ⊗ (g ⊗ h)
f ⊗ g ⊗ h

-- Non-target: (f ⊗ g) ⊗ h
(f ⊗ g) ⊗ h
```

### Unitors

**Target**: No redundant unit objects tensored with a morphism when unitors can be eliminated.

```lean
-- Non-target
𝟙_ C ⊗ f
f ⊗ 𝟙_ C

-- Target
f
```

**Rule (design)**: `tensor_id`, `id_tensor`, and related lemmas. Segment passes also rewrite `tensor .id g` and `tensor f .id`.

### Associators

**Target**: Prefer explicit right-associated tensors instead of standalone associator morphisms when coherence allows.

```lean
-- Non-target: explicit associator
α_ _ _ _

-- Target: right-associated tensor spine
f ⊗ (g ⊗ h)
```

### Braiding (braided categories)

**Target**: A canonical reordering of tensor factors (minimal permutation relative to the chosen ordering strategy).

**Rule (design)**: Depends on the braided structure; deep hexagon-style rewrites are largely **not** hooked into `normalizeGoalM` yet.

### Symmetry (symmetric monoidal categories)

**Target**: Often a **fixed** order on tensor factors (for example lexicographic), so symmetric swaps have a definite direction.

---

## Examples

### Composition and identities (design intent)

```lean
-- Before (design): identities and mixed association
(f ≫ 𝟙 Y) ≫ (g ≫ h) ≫ 𝟙 Z

-- After (design): right-associated, identities gone
f ≫ g ≫ h
```

Achieving this in practice today requires running **`normalizeAssocUnit`** (or a future unified pipeline), not `normalizeGoalM` alone.

### Tensor and composition (read carefully)

Composition and tensor bind with different precedences in Lean. Parentheses matter in real goals.

```lean
-- Before: left-associated tensor spine (conceptual)
(f ⊗ g) ⊗ h

-- After: right-associated tensor
f ⊗ (g ⊗ h)
```

Mixing `⊗` and `≫` in one expression should be written with explicit parentheses in both source and documentation to avoid ambiguity.

### Isomorphism cancellation (segment-level)

```lean
-- Before: cancellable iso legs around f (when iso matches)
iso.hom ≫ f ≫ iso.inv

-- After (design / segment passes when wired): simplified to f
f
```

---

## Determinism

The project is **designed** for predictable normalization:

1. **Fixed pass order** when multiple passes are composed explicitly.
2. **Stable structural equality** on `ExprSegment` (see `BEq` instance in `CatNF.Core`) for comparing segment lists in tests.
3. **Bounded work**: configuration limits (`maxSteps`, `timeoutMs`, segment counts) and depth limits during flattening.
4. **Single-pass** segment loops where implemented (no search/backtracking).

Calling **`normalizeGoalM` twice** on the same `Expr` with the same `Config` should yield the same result for the current flatten–rebuild behavior. Stronger claims (“every equivalent morphism normalizes to the same expression”) require the full rewrite stack and Mathlib definitional equality; those are **goals**, not promises of the current minimal pipeline.

---

## See also

- [UserGuide.md](UserGuide.md) — which API to call, config limits, and troubleshooting.
- [Compatibility.md](Compatibility.md) — Lean / Mathlib versions.
- [README.md](../README.md) — `Config`, error types, and module imports.
