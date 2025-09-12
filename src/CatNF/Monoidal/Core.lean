import Mathlib.CategoryTheory.Category.Basic
import Mathlib.CategoryTheory.Functor.Basic
import Mathlib.CategoryTheory.Iso
import Mathlib.CategoryTheory.Monoidal.Category
import Mathlib.CategoryTheory.Monoidal.Braided
import Mathlib.CategoryTheory.Monoidal.Symmetric
import Mathlib.Data.List.Basic
import Mathlib.Data.Array.Basic
import Lean.Expr
import Lean.Meta
import Lean.Elab.Command
import Mathlib.Tactic.Basic
import Mathlib.Tactic.SimpRw

namespace CatNF.Monoidal

-- Monoidal category normalization

-- Apply associator rules for monoidal categories
def applyAssociators (segments : List ExprSegment) : MetaM (List ExprSegment) := do
  let mut result := segments
  let mut changed := true

  while changed do
    changed := false
    let mut newResult := []

    for i in [0:result.length] do
      let seg := result[i]!
      match seg with
      | .associator f g h =>
        -- α_ f g h = (f ⊗ g) ⊗ h = f ⊗ (g ⊗ h)
        let leftTensor := ExprSegment.tensor (ExprSegment.tensor f g) h
        let rightTensor := ExprSegment.tensor f (ExprSegment.tensor g h)
        -- Choose canonical form (right-associated)
        newResult := newResult ++ [rightTensor]
        changed := true
      | .tensor (.tensor f g) h =>
        -- Left-associate tensors: (f ⊗ g) ⊗ h = f ⊗ (g ⊗ h)
        let newTensor := ExprSegment.tensor f (ExprSegment.tensor g h)
        newResult := newResult ++ [newTensor]
        changed := true
      | _ =>
        newResult := newResult ++ [seg]

    result := newResult

  return result

-- Apply unitor rules for monoidal categories
def applyUnitors (segments : List ExprSegment) : MetaM (List ExprSegment) := do
  let mut result := segments
  let mut changed := true

  while changed do
    changed := false
    let mut newResult := []

    for i in [0:result.length] do
      let seg := result[i]!
      match seg with
      | .left_unitor f =>
        -- λ_ f = 𝟙 ⊗ f
        let newTensor := ExprSegment.tensor .id f
        newResult := newResult ++ [newTensor]
        changed := true
      | .right_unitor f =>
        -- ρ_ f = f ⊗ 𝟙
        let newTensor := ExprSegment.tensor f .id
        newResult := newResult ++ [newTensor]
        changed := true
      | .tensor .id g =>
        -- 𝟙 ⊗ g = g (tensor_id)
        newResult := newResult ++ [g]
        changed := true
      | .tensor f .id =>
        -- f ⊗ 𝟙 = f (id_tensor)
        newResult := newResult ++ [f]
        changed := true
      | _ =>
        newResult := newResult ++ [seg]

    result := newResult

  return result

-- Apply braiding rules for braided monoidal categories
def applyBraiding (segments : List ExprSegment) : MetaM (List ExprSegment) := do
  let mut result := segments
  let mut changed := true

  while changed do
    changed := false
    let mut newResult := []

    for i in [0:result.length] do
      let seg := result[i]!
      match seg with
      | .braid f g =>
        -- β_ f g = f ⊗ g = g ⊗ f (braiding)
        let newTensor := ExprSegment.tensor g f
        newResult := newResult ++ [newTensor]
        changed := true
      | _ =>
        newResult := newResult ++ [seg]

    result := newResult

  return result

-- Apply symmetry rules for symmetric monoidal categories
def applySymmetry (segments : List ExprSegment) : MetaM (List ExprSegment) := do
  let mut result := segments
  let mut changed := true

  while changed do
    changed := false
    let mut newResult := []

    for i in [0:result.length] do
      let seg := result[i]!
      match seg with
      | .braid f g =>
        -- In symmetric categories, β_ f g = β_ g f
        -- Choose canonical form (lexicographic order)
        if f.toString < g.toString then
          let newTensor := ExprSegment.tensor f g
          newResult := newResult ++ [newTensor]
        else
          let newTensor := ExprSegment.tensor g f
          newResult := newResult ++ [newTensor]
        changed := true
      | _ =>
        newResult := newResult ++ [seg]

    result := newResult

  return result

-- Main monoidal normalization
def normalizeMonoidal (segments : List ExprSegment) : MetaM (List ExprSegment) := do
  let step1 ← applyAssociators segments
  let step2 ← applyUnitors step1
  let step3 ← applyBraiding step2
  let step4 ← applySymmetry step3
  return step4

-- Apply monoidal lemmas to an expression
def applyMonoidalLemmas (expr : Expr) : MetaM Expr := do
  try
    simpOnly [
      CategoryTheory.MonoidalCategory.tensor_id,
      CategoryTheory.MonoidalCategory.id_tensor,
      CategoryTheory.MonoidalCategory.tensor_comp,
      CategoryTheory.MonoidalCategory.associator_naturality,
      CategoryTheory.MonoidalCategory.leftUnitor_naturality,
      CategoryTheory.MonoidalCategory.rightUnitor_naturality
    ] expr
  catch _ =>
    return expr

-- Apply braiding lemmas to an expression
def applyBraidingLemmas (expr : Expr) : MetaM Expr := do
  try
    simpOnly [
      CategoryTheory.MonoidalCategory.braiding_naturality,
      CategoryTheory.MonoidalCategory.braiding_tensor,
      CategoryTheory.MonoidalCategory.braiding_hexagon_forward,
      CategoryTheory.MonoidalCategory.braiding_hexagon_reverse
    ] expr
  catch _ =>
    return expr

-- Check if two expressions are monoidally equivalent
def areMonoidallyEquivalent (e1 e2 : Expr) : MetaM Bool := do
  let segs1 ← flattenComposition e1
  let segs2 ← flattenComposition e2
  let norm1 ← normalizeMonoidal segs1
  let norm2 ← normalizeMonoidal segs2
  return norm1 == norm2

-- Extract monoidal structure from an expression
def extractMonoidalStructure (expr : Expr) : MetaM (Option (Expr × Expr)) := do
  match expr with
  | .app (.app (.const `CategoryTheory.MonoidalCategory.tensorObj _) f) g => return some (f, g)
  | _ => return none

-- Check if an expression involves monoidal structure
def involvesMonoidalStructure (expr : Expr) : MetaM Bool := do
  match ← extractMonoidalStructure expr with
  | some _ => return true
  | none => return false

-- Apply all monoidal rules
def applyAllMonoidalRules (expr : Expr) : MetaM Expr := do
  let step1 ← applyMonoidalLemmas expr
  let step2 ← applyBraidingLemmas step1
  return step2

end CatNF.Monoidal
