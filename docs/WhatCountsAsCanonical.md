# What counts as canonical?

This document describes **target** normal forms the CatNF pipeline aims toward. For how to run normalization in code, see [README.md](../README.md). The examples below are **illustrative** and may not match exact Mathlib pretty-printing.

## Categories

### Composition

**Canonical**: Right-associated compositions
```lean
-- Canonical: f ≫ (g ≫ h)
f ≫ g ≫ h

-- Non-canonical: (f ≫ g) ≫ h
(f ≫ g) ≫ h
```

**Rule**: All compositions are right-associated using associativity.

### Identity Morphisms

**Canonical**: No identity morphisms
```lean
-- Canonical: f
f

-- Non-canonical: f ≫ 𝟙 Y
f ≫ 𝟙 Y

-- Non-canonical: 𝟙 X ≫ f
𝟙 X ≫ f
```

**Rule**: Identity morphisms are removed using `id_comp` and `comp_id`.

### Isomorphisms

**Target**: Cancel adjacent `hom ≫ inv` and `inv ≫ hom` pairs down to identity when the iso laws apply.

```lean
-- Non-canonical (reducible): composed iso legs
iso.hom ≫ iso.inv

-- Target: identity on the domain (conceptually 𝟙 _)
-- (Exact lemma names depend on Mathlib’s iso API)
```

**Rule**: Use isomorphism laws (`hom_inv_id`, `inv_hom_id`, etc.) so explicit `hom ≫ inv` / `inv ≫ hom` chains do not remain in the normalized morphism expression.

### Functor Maps

**Canonical**: Flattened `map_comp` chains
```lean
-- Canonical: F.map f ≫ F.map g
F.map (f ≫ g)

-- Non-canonical: F.map (f ≫ g)
F.map (f ≫ g)
```

**Rule**: Functor maps are flattened using `map_comp`.

### Whiskering

**Canonical**: Standardized whiskering order
```lean
-- Canonical: (F ◁ f) ≫ (F ◁ g)
F ◁ (f ≫ g)

-- Canonical: (f ▷ G) ≫ (g ▷ G)
(f ≫ g) ▷ G
```

**Rule**: Whiskering is standardized using `whiskerLeft_comp` and `whiskerRight_comp`.

## Monoidal categories

Monoidal, braided, and symmetric behavior depends on the concrete `MonoidalCategory` / `BraidedCategory` instance and on which rewrites are available. The following is the **intended** shape of normal forms, not a guarantee for every morphism type.

### Tensor products

**Canonical**: Right-associated tensors
```lean
-- Canonical: f ⊗ (g ⊗ h)
f ⊗ g ⊗ h

-- Non-canonical: (f ⊗ g) ⊗ h
(f ⊗ g) ⊗ h
```

**Rule**: All tensors are right-associated using associativity.

### Unitors

**Canonical**: No unitors
```lean
-- Canonical: f
𝟙_ C ⊗ f

-- Canonical: f
f ⊗ 𝟙_ C
```

**Rule**: Unitors are removed using `tensor_id` and `id_tensor`.

### Associators

**Canonical**: No associators
```lean
-- Canonical: f ⊗ (g ⊗ h)
(f ⊗ g) ⊗ h
```

**Rule**: Associators are replaced by right-association.

### Braiding (Braided Categories)

**Canonical**: Minimal permutation form
```lean
-- Canonical: g ⊗ f
f ⊗ g

-- Canonical: h ⊗ g ⊗ f
f ⊗ g ⊗ h
```

**Rule**: Braids are normalized to a canonical minimal permutation form.

### Symmetry (Symmetric Categories)

**Canonical**: Lexicographic order
```lean
-- Canonical: g ⊗ f
f ⊗ g

-- Canonical: h ⊗ g ⊗ f
f ⊗ g ⊗ h
```

**Rule**: Symmetries are normalized to lexicographic order.

## Examples

### Before and After Normalization

```lean
-- Before: Complex composition with identities and associators
(f ≫ 𝟙 Y) ≫ (g ≫ h) ≫ 𝟙 Z

-- After: Clean right-associated form
f ≫ g ≫ h
```

```lean
-- Before: Mixed tensor and composition
(f ⊗ g) ≫ (h ⊗ i) ≫ (j ⊗ k)

-- After: Normalized form
f ⊗ g ≫ h ⊗ i ≫ j ⊗ k
```

```lean
-- Before: Isomorphism with cancellation
iso.hom ≫ f ≫ iso.inv

-- After: Simplified form
f
```

## Determinism

The normalizer ensures determinism through:

1. **Fixed pass order**: Always apply rules in the same sequence
2. **Stable sorting**: Use consistent ordering for equivalent expressions
3. **No backtracking**: Single-pass normalization
4. **Bounded steps**: Prevent infinite loops
5. **Local simp sets**: Only use registered rules

This guarantees that equivalent expressions always normalize to the same canonical form.
