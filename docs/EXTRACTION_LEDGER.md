# CatNF lemma extraction ledger

This ledger records normalization cases exercised by CatNF tests and category frontends on the Lean 4.31 / Mathlib `v4.31.0-rc1` line. It is the first upstream artifact for Mathlib lemma and documentation PRs. No `cat_nf` tactic syntax is proposed here.

## Legend

| Column | Meaning |
|--------|---------|
| Input goal | Morphism expression shape before normalization |
| Normalization step | CatNF pipeline stage |
| Existing Mathlib theorem | Lemma already available for the rewrite |
| Missing theorem | Gap blocking `simp`/`reassoc` parity |
| Candidate statement | Proposed Mathlib lemma |
| Proposed Mathlib file | Target location |
| Proof sketch | Expected proof route |
| Review risk | Maintainer review concern |

---

## Case 1 — Identity cancellation in a composition chain

| Field | Value |
|-------|-------|
| Input goal | `f ≫ 𝟙 ≫ g` (flattened segment list `[f, id, g]`) |
| Normalization step | `removeIdentities` / `eraseIdentities` |
| Existing Mathlib theorem | `Category.id_comp`, `Category.comp_id` |
| Missing theorem | None for bare identities; ledger notes need for `reassoc`-friendly one-liner |
| Candidate statement | `example {C : Type*} [Category C] {X Y Z : C} (f : X ⟶ Y) (g : Y ⟶ Z) : f ≫ g = f ≫ 𝟙 Y ≫ g := by simp` |
| Proposed Mathlib file | `Mathlib/CategoryTheory/Category/Basic.lean` (documentation example block) |
| Proof sketch | `simp` |
| Review risk | Low — documentation only |

Source: `testRemoveIdentities`, `testEraseIdentities` in `src/CatNF/Tests/Unit/Core.lean`.

---

## Case 2 — Right-associated composition normal form

| Field | Value |
|-------|-------|
| Input goal | Flat segments `[f, g, h]` |
| Normalization step | `rightAssociate` / `applyAssociativity` |
| Existing Mathlib theorem | `Category.assoc` |
| Missing theorem | `Category.assoc_reassoc` variant listing left-to-right factorization for long chains |
| Candidate statement | `theorem comp_assoc_reassoc {C} [Category C] {W X Y Z : C} (f : W ⟶ X) (g : X ⟶ Y) (h : Y ⟶ Z) (k : Z ⟶ W) : (f ≫ g) ≫ (h ≫ k) = f ≫ (g ≫ h) ≫ k := by simp [← Category.assoc]` |
| Proposed Mathlib file | `Mathlib/CategoryTheory/Category/Basic.lean` |
| Proof sketch | Repeated `assoc` + `simp` |
| Review risk | Low–medium — must match existing `reassoc` conventions |

Source: `testRightAssociate`, `testApplyAssociativity`.

---

## Case 3 — Functor map over identity

| Field | Value |
|-------|-------|
| Input goal | `F.map (𝟙 X)` |
| Normalization step | `flattenMapComp` |
| Existing Mathlib theorem | `Functor.map_id` |
| Missing theorem | None |
| Candidate statement | Documentation example: `example (F : C ⥤ D) (X : C) : F.map (𝟙 X) = 𝟙 (F.obj X) := Functor.map_id F X` |
| Proposed Mathlib file | `Mathlib/CategoryTheory/Functor/Basic.lean` |
| Proof sketch | `rfl` / `Functor.map_id` |
| Review risk | Low |

Source: `flattenMapComp` branch on `functor_map _ .id` in `src/CatNF/Category/FunctorWhisker.lean`.

---

## Case 4 — Functor map over composition

| Field | Value |
|-------|-------|
| Input goal | `F.map (f ≫ g)` |
| Normalization step | `flattenMapComp` |
| Existing Mathlib theorem | `Functor.map_comp` |
| Missing theorem | `Functor.map_comp_assoc` simp lemma for nested compositions in long chains |
| Candidate statement | `@[simp] lemma map_comp_assoc {C D} [Category C] [Category D] (F : C ⥤ D) {X Y Z W : C} (f : X ⟶ Y) (g : Y ⟶ Z) (h : Z ⟶ W) : F.map (f ≫ g ≫ h) = F.map f ≫ F.map g ≫ F.map h := by simp [Functor.map_comp]` |
| Proposed Mathlib file | `Mathlib/CategoryTheory/Functor/Basic.lean` |
| Proof sketch | Two applications of `Functor.map_comp` |
| Review risk | Medium — simp set size |

Source: `flattenMapComp` on `functor_map F (.comp f g)`.

---

## Case 5 — Merge adjacent functor maps

| Field | Value |
|-------|-------|
| Input goal | Segment list `[F.map f, F.map g]` |
| Normalization step | `applyFunctoriality` |
| Existing Mathlib theorem | `Functor.map_comp` (conceptually) |
| Missing theorem | `Functor.map_comp_symm` for reversed segment order if normal form is right-associated |
| Candidate statement | `lemma map_comp_of_adjacent {C D} [Category C] [Category D] (F : C ⥤ D) {X Y Z : C} (f : X ⟶ Y) (g : Y ⟶ Z) : F.map f ≫ F.map g = F.map (f ≫ g) := (Functor.map_comp F).symm` |
| Proposed Mathlib file | `Mathlib/CategoryTheory/Functor/Basic.lean` |
| Proof sketch | `exact (Functor.map_comp F).symm` |
| Review risk | Low |

Source: `applyFunctoriality` in `FunctorWhisker.lean`.

---

## Case 6 — Whiskering over composition (left)

| Field | Value |
|-------|-------|
| Input goal | `whiskerLeft F (f ≫ g)` (segment `whisker_left F (.comp f g)`) |
| Normalization step | `standardizeWhiskering` |
| Existing Mathlib theorem | `Functor.whiskerLeft_comp` (Mathlib 4.31 naming) |
| Missing theorem | Confirm stable `simp` lemma for `whiskerLeft` distributing over `≫` in normal-form pipelines |
| Candidate statement | `@[simp] lemma whiskerLeft_comp {C D E} [Category C] [Category D] [Category E] (F : C ⥤ D) {G H : D ⥤ E} (f : G ⟶ H) (g : H ⟶ I) : whiskerLeft F (f ≫ g) = whiskerLeft F f ≫ whiskerLeft F g := whiskerLeft_comp F f g` |
| Proposed Mathlib file | `Mathlib/CategoryTheory/Whiskering.lean` |
| Proof sketch | `whiskerLeft_comp` |
| Review risk | Low — resolved via `CatNF.MorphismNames` (`Functor.whiskerLeft` / `whiskerRight` on Mathlib 4.31) |

Source: `standardizeWhiskering` in `FunctorWhisker.lean`.

---

## Case 7 — Whiskering over identity

| Field | Value |
|-------|-------|
| Input goal | `whiskerLeft F (𝟙 G)` |
| Normalization step | `standardizeWhiskering` |
| Existing Mathlib theorem | `Functor.whiskerLeft_id` |
| Missing theorem | None |
| Candidate statement | `example (F : C ⥤ D) (G : D ⥤ E) : whiskerLeft F (𝟙 G) = 𝟙 (F ⋙ G) := whiskerLeft_id F` |
| Proposed Mathlib file | `Mathlib/CategoryTheory/Whiskering.lean` |
| Proof sketch | `whiskerLeft_id` |
| Review risk | Low |

Source: `standardizeWhiskering` branch `whisker_left _ .id`.

---

## Case 8 — Iso hom / inv cancellation

| Field | Value |
|-------|-------|
| Input goal | `e.hom ≫ e.inv` |
| Normalization step | `shuntIsomorphisms` / `normalizeIsoCompositions` |
| Existing Mathlib theorem | `Iso.hom_inv_id`, `Iso.inv_hom_id` |
| Missing theorem | None for single iso; missing chained `Iso.trans` reassoc simp for `[hom e1, inv e1, hom e2, inv e2]` |
| Candidate statement | `@[simp] lemma hom_inv_trans {C} [Category C] {X Y Z : C} (e : X ≅ Y) (f : Y ≅ Z) : e.hom ≫ e.inv ≫ f.hom ≫ f.inv = 𝟙 X := by simp [Iso.hom_inv_id, Category.assoc]` |
| Proposed Mathlib file | `Mathlib/CategoryTheory/Iso/Basic.lean` |
| Proof sketch | Cancel each `hom ≫ inv` pair |
| Review risk | Medium — depends on simp order |

Source: `testShuntIsomorphisms`, `normalizeIsoCompositions` in `IsoTransport.lean`.

---

## Case 9 — Iso hom followed by inv in composition flattening

| Field | Value |
|-------|-------|
| Input goal | `comp (iso_hom iso) (iso_inv iso)` |
| Normalization step | `normalizeIsoCompositions` |
| Existing Mathlib theorem | `Iso.hom_inv_id` |
| Missing theorem | None |
| Candidate statement | `example {C} [Category C] {X Y : C} (e : X ≅ Y) : e.hom ≫ e.inv = 𝟙 X := Iso.hom_inv_id e` |
| Proposed Mathlib file | `Mathlib/CategoryTheory/Iso/Basic.lean` |
| Proof sketch | `Iso.hom_inv_id` |
| Review risk | Low |

Source: `normalizeIsoCompositions` hom/inv pair branch.

---

## Case 10 — Functoriality of iso hom

| Field | Value |
|-------|-------|
| Input goal | `F.map e.hom` |
| Normalization step | `applyIsoFunctoriality` |
| Existing Mathlib theorem | `Functor.mapIso_hom` |
| Missing theorem | None |
| Candidate statement | `example (F : C ⥤ D) {X Y : C} (e : X ≅ Y) : F.map e.hom = (F.mapIso e).hom := (Functor.mapIso_hom F e).symm` |
| Proposed Mathlib file | `Mathlib/CategoryTheory/Functor/Basic.lean` |
| Proof sketch | `Functor.mapIso_hom` |
| Review risk | Low |

Source: `applyIsoFunctoriality` in `IsoTransport.lean`.

---

## Case 11 — Naturality square reassociation (whisker commute)

| Field | Value |
|-------|-------|
| Input goal | `whiskerLeft F β ≫ whiskerRight α K` vs swapped order |
| Normalization step | `applyWhiskeringCommutation` |
| Existing Mathlib theorem | `NatTrans.hcomp_eq_whiskerLeft_comp_whiskerRight`, `whiskerLeft_comp_whiskerRight` |
| Missing theorem | `simp` lemma packaging exchange for normal-form sorting |
| Candidate statement | `lemma whisker_exchange {C D E} [Category C] [Category D] [Category E] (F G : C ⥤ D) (α : F ⟶ G) (H K : D ⥤ E) (β : H ⟶ K) : whiskerLeft F β ≫ whiskerRight α K = whiskerRight α H ≫ whiskerLeft G β := whiskerLeft_comp_whiskerRight α β` |
| Proposed Mathlib file | `Mathlib/CategoryTheory/Whiskering.lean` |
| Proof sketch | `whiskerLeft_comp_whiskerRight` |
| Review risk | Medium — easy to duplicate existing hcomp lemmas |

Source: `applyWhiskeringCommutation` in `FunctorWhisker.lean`.

---

## Case 12 — Composition flattening preserves factors

| Field | Value |
|-------|-------|
| Input goal | `(f ≫ g) ≫ h` |
| Normalization step | `flattenComposition` |
| Existing Mathlib theorem | `Category.assoc` (not used at flatten stage) |
| Missing theorem | None at extraction stage; documentation example for "list of morphisms" normal forms |
| Candidate statement | `example {C} [Category C] {W X Y Z : C} (f : W ⟶ X) (g : X ⟶ Y) (h : Y ⟶ Z) : (f ≫ g) ≫ h = f ≫ (g ≫ h) := (Category.assoc f g h).symm` |
| Proposed Mathlib file | `Mathlib/CategoryTheory/Category/Basic.lean` |
| Proof sketch | `Category.assoc` |
| Review risk | Low |

Source: `testFlattenComposition`.

---

## Deferred / experimental (not blocking core port)

| Area | Module | Status on 4.31 |
|------|--------|----------------|
| Monoidal associator unfolding | `CatNF.Monoidal.Core` | Experimental; not in narrow-scope build |
| Braided/tensor unit rules | `CatNF.Monoidal.Coherence` | Experimental |
| Parallel/cache layers | `CatNF.ParallelProcessing`, `CatNF.Cache` | Repository-local infrastructure |

---

## Architecture (kernel vs category pipeline)

| Layer | Module | Role |
|-------|--------|------|
| Morphism names | `CatNF/Core/MorphismNames.lean` | Single source of truth for `CategoryTheory.*` head symbols (Mathlib 4.31: `Functor.whiskerLeft` / `whiskerRight`) |
| Kernel | `CatNF/Core/Normalize.lean` | Flatten / rebuild; no Mathlib |
| Category frontends | `CatNF/Category/{Basic,FunctorWhisker,IsoTransport}.lean` | Associativity, functor/whisker, iso transport |
| Pipeline | `CatNF/Category/Pipeline.lean` | `normalizeGoalM`: flatten → category steps → rebuild |

---

## Build certification (this sprint)

| Command | Status (2026-06-09, Windows) |
|---------|--------------------------------|
| `lake update` | Pass (`elan run leanprover/lean4:v4.31.0-rc1 -- lake update`) |
| `lake build` | Pass — default `CatNF` lib (524 jobs) |
| `lake build CatNFTests` | Pass — all test modules including `CatNF.Tests.TestRunner` (854 jobs) |
| `lake exe test-runner` | Pass — 11/11 tests (console subsystem link fix in `Lakefile.lean`) |
| `lake exe test-runner-final` | Pass — kernel + pipeline smoke |
| `lake exe bench` | Pass — after Lean 4.31 `withImportModules` API fix |

Remaining experimental scope (not blocking narrow-scope certification):

- **Monoidal**: `CatNF.Monoidal.Coherence` builds but remains experimental / out of narrow-scope normalization.
- **Parallel Windows builds**: occasional `olean.private` read races under heavy parallelism; retry `lake build` if transient.
