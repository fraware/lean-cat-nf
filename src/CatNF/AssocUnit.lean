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

namespace CatNF

-- Associativity and unit handling for categorical compositions

-- Right-associate a list of composition segments
def rightAssociate (segments : List ExprSegment) : List ExprSegment :=
  let rec aux (acc : List ExprSegment) (remaining : List ExprSegment) : List ExprSegment :=
    match remaining with
    | [] => acc.reverse
    | [seg] => (seg :: acc).reverse
    | seg1 :: seg2 :: rest =>
      let comp := ExprSegment.comp seg1 seg2
      aux (comp :: acc) rest
  aux [] segments

-- Remove identity morphisms using local lemmas
def removeIdentities (segments : List ExprSegment) : MetaM (List ExprSegment) := do
  let mut result := segments
  let mut changed := true

  while changed do
    changed := false
    let mut newResult := []

    for i in [0:result.length] do
      let seg := result[i]!
      match seg with
      | .id =>
        -- Skip identity morphisms
        changed := true
      | .comp f .id =>
        -- f ≫ 𝟙 = f (comp_id)
        newResult := newResult ++ [f]
        changed := true
      | .comp .id g =>
        -- 𝟙 ≫ g = g (id_comp)
        newResult := newResult ++ [g]
        changed := true
      | .comp (.comp f g) h =>
        -- (f ≫ g) ≫ h = f ≫ (g ≫ h) (associativity)
        let newComp := ExprSegment.comp f (ExprSegment.comp g h)
        newResult := newResult ++ [newComp]
        changed := true
      | _ =>
        newResult := newResult ++ [seg]

    result := newResult

  return result

-- Apply associativity rules to normalize composition structure
def applyAssociativity (segments : List ExprSegment) : MetaM (List ExprSegment) := do
  let mut result := segments
  let mut changed := true

  while changed do
    changed := false
    let mut newResult := []

    for i in [0:result.length] do
      let seg := result[i]!
      match seg with
      | .comp (.comp f g) h =>
        -- Right-associate: (f ≫ g) ≫ h = f ≫ (g ≫ h)
        let newComp := ExprSegment.comp f (ExprSegment.comp g h)
        newResult := newResult ++ [newComp]
        changed := true
      | _ =>
        newResult := newResult ++ [seg]

    result := newResult

  return result

-- Handle unit morphisms in monoidal categories
def handleUnits (segments : List ExprSegment) : MetaM (List ExprSegment) := do
  let mut result := segments
  let mut changed := true

  while changed do
    changed := false
    let mut newResult := []

    for i in [0:result.length] do
      let seg := result[i]!
      match seg with
      | .tensor .id g =>
        -- 𝟙 ⊗ g = g (tensor_id)
        newResult := newResult ++ [g]
        changed := true
      | .tensor f .id =>
        -- f ⊗ 𝟙 = f (id_tensor)
        newResult := newResult ++ [f]
        changed := true
      | .left_unitor f =>
        -- λ_ f = 𝟙 ⊗ f (left_unitor)
        let newTensor := ExprSegment.tensor .id f
        newResult := newResult ++ [newTensor]
        changed := true
      | .right_unitor f =>
        -- ρ_ f = f ⊗ 𝟙 (right_unitor)
        let newTensor := ExprSegment.tensor f .id
        newResult := newResult ++ [newTensor]
        changed := true
      | _ =>
        newResult := newResult ++ [seg]

    result := newResult

  return result

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

-- Main associativity and unit normalization
def normalizeAssocUnit (segments : List ExprSegment) : MetaM (List ExprSegment) := do
  let step1 ← removeIdentities segments
  let step2 ← applyAssociativity step1
  let step3 ← handleUnits step2
  let step4 ← applyAssociators step3
  return step4

-- Check if two expressions are associatively equivalent
def areAssociativelyEquivalent (e1 e2 : Expr) : MetaM Bool := do
  let segs1 ← flattenComposition e1
  let segs2 ← flattenComposition e2
  let norm1 ← normalizeAssocUnit segs1
  let norm2 ← normalizeAssocUnit segs2
  return norm1 == norm2

-- Apply associativity lemmas to an expression
def applyAssociativityLemmas (expr : Expr) : MetaM Expr := do
  try
    simpOnly [CategoryTheory.Category.assoc] expr
  catch _ =>
    return expr

-- Apply unit lemmas to an expression
def applyUnitLemmas (expr : Expr) : MetaM Expr := do
  try
    simpOnly [CategoryTheory.Category.id_comp, CategoryTheory.Category.comp_id] expr
  catch _ =>
    return expr

end CatNF
