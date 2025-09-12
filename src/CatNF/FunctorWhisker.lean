import Mathlib.CategoryTheory.Category.Basic
import Mathlib.CategoryTheory.Functor.Basic
import Mathlib.CategoryTheory.Iso
import Mathlib.CategoryTheory.Monoidal.Category
import Mathlib.CategoryTheory.Monoidal.Braided
import Mathlib.CategoryTheory.Monoidal.Symmetric
import Mathlib.CategoryTheory.Whiskering
import Mathlib.Data.List.Basic
import Mathlib.Data.Array.Basic
import Lean.Expr
import Lean.Meta
import Lean.Elab.Command
import Mathlib.Tactic.Basic
import Mathlib.Tactic.SimpRw

namespace CatNF

-- Functor and whiskering normalization

-- Flatten map_comp chains
def flattenMapComp (segments : List ExprSegment) : MetaM (List ExprSegment) := do
  let mut result := segments
  let mut changed := true

  while changed do
    changed := false
    let mut newResult := []

    for i in [0:result.length] do
      let seg := result[i]!
      match seg with
      | .functor_map F (.comp f g) =>
        -- F.map (f ≫ g) = F.map f ≫ F.map g
        let fMap := ExprSegment.functor_map F f
        let gMap := ExprSegment.functor_map F g
        let newComp := ExprSegment.comp fMap gMap
        newResult := newResult ++ [newComp]
        changed := true
      | .functor_map F (.id) =>
        -- F.map 𝟙 = 𝟙
        newResult := newResult ++ [.id]
        changed := true
      | _ =>
        newResult := newResult ++ [seg]

    result := newResult

  return result

-- Standardize whiskering order
def standardizeWhiskering (segments : List ExprSegment) : MetaM (List ExprSegment) := do
  let mut result := segments
  let mut changed := true

  while changed do
    changed := false
    let mut newResult := []

    for i in [0:result.length] do
      let seg := result[i]!
      match seg with
      | .whisker_left F (.comp f g) =>
        -- F ◁ (f ≫ g) = (F ◁ f) ≫ (F ◁ g)
        let fWhisker := ExprSegment.whisker_left F f
        let gWhisker := ExprSegment.whisker_left F g
        let newComp := ExprSegment.comp fWhisker gWhisker
        newResult := newResult ++ [newComp]
        changed := true
      | .whisker_right (.comp f g) G =>
        -- (f ≫ g) ▷ G = (f ▷ G) ≫ (g ▷ G)
        let fWhisker := ExprSegment.whisker_right f G
        let gWhisker := ExprSegment.whisker_right g G
        let newComp := ExprSegment.comp fWhisker gWhisker
        newResult := newResult ++ [newComp]
        changed := true
      | .whisker_left F (.id) =>
        -- F ◁ 𝟙 = 𝟙
        newResult := newResult ++ [.id]
        changed := true
      | .whisker_right (.id) G =>
        -- 𝟙 ▷ G = 𝟙
        newResult := newResult ++ [.id]
        changed := true
      | _ =>
        newResult := newResult ++ [seg]

    result := newResult

  return result

-- Apply functoriality rules
def applyFunctoriality (segments : List ExprSegment) : MetaM (List ExprSegment) := do
  let mut result := segments
  let mut changed := true

  while changed do
    changed := false
    let mut newResult := []

    for i in [0:result.length] do
      let seg := result[i]!
      match seg with
      | .comp (.functor_map F f) (.functor_map F g) =>
        -- F.map f ≫ F.map g = F.map (f ≫ g)
        let newMap := ExprSegment.functor_map F (ExprSegment.comp f g)
        newResult := newResult ++ [newMap]
        changed := true
      | .comp (.whisker_left F f) (.whisker_left F g) =>
        -- (F ◁ f) ≫ (F ◁ g) = F ◁ (f ≫ g)
        let newWhisker := ExprSegment.whisker_left F (ExprSegment.comp f g)
        newResult := newResult ++ [newWhisker]
        changed := true
      | .comp (.whisker_right f G) (.whisker_right g G) =>
        -- (f ▷ G) ≫ (g ▷ G) = (f ≫ g) ▷ G
        let newWhisker := ExprSegment.whisker_right (ExprSegment.comp f g) G
        newResult := newResult ++ [newWhisker]
        changed := true
      | _ =>
        newResult := newResult ++ [seg]

    result := newResult

  return result

-- Apply whiskering commutation rules
def applyWhiskeringCommutation (segments : List ExprSegment) : MetaM (List ExprSegment) := do
  let mut result := segments
  let mut changed := true

  while changed do
    changed := false
    let mut newResult := []

    for i in [0:result.length] do
      let seg := result[i]!
      match seg with
      | .comp (.whisker_left F f) (.whisker_right g G) =>
        -- (F ◁ f) ≫ (g ▷ G) = (g ▷ G) ≫ (F ◁ f) (whiskering commutes)
        let newComp := ExprSegment.comp (.whisker_right g G) (.whisker_left F f)
        newResult := newResult ++ [newComp]
        changed := true
      | .comp (.whisker_right f G) (.whisker_left F g) =>
        -- (f ▷ G) ≫ (F ◁ g) = (F ◁ g) ≫ (f ▷ G) (whiskering commutes)
        let newComp := ExprSegment.comp (.whisker_left F g) (.whisker_right f G)
        newResult := newResult ++ [newComp]
        changed := true
      | _ =>
        newResult := newResult ++ [seg]

    result := newResult

  return result

-- Main functor and whiskering normalization
def normalizeFunctorWhisker (segments : List ExprSegment) : MetaM (List ExprSegment) := do
  let step1 ← flattenMapComp segments
  let step2 ← standardizeWhiskering step1
  let step3 ← applyFunctoriality step2
  let step4 ← applyWhiskeringCommutation step3
  return step4

-- Apply functor lemmas to an expression
def applyFunctorLemmas (expr : Expr) : MetaM Expr := do
  try
    simpOnly [
      CategoryTheory.Functor.map_id,
      CategoryTheory.Functor.map_comp,
      CategoryTheory.whiskerLeft_id,
      CategoryTheory.whiskerRight_id,
      CategoryTheory.whiskerLeft_comp,
      CategoryTheory.whiskerRight_comp
    ] expr
  catch _ =>
    return expr

-- Apply whiskering lemmas to an expression
def applyWhiskeringLemmas (expr : Expr) : MetaM Expr := do
  try
    simpOnly [
      CategoryTheory.whiskerLeft_comp,
      CategoryTheory.whiskerRight_comp,
      CategoryTheory.whiskerLeft_id,
      CategoryTheory.whiskerRight_id,
      CategoryTheory.whiskerLeft_whiskerRight,
      CategoryTheory.whiskerRight_whiskerLeft
    ] expr
  catch _ =>
    return expr

-- Check if two expressions are functorially equivalent
def areFunctoriallyEquivalent (e1 e2 : Expr) : MetaM Bool := do
  let segs1 ← flattenComposition e1
  let segs2 ← flattenComposition e2
  let norm1 ← normalizeFunctorWhisker segs1
  let norm2 ← normalizeFunctorWhisker segs2
  return norm1 == norm2

-- Extract functor from an expression
def extractFunctor (expr : Expr) : MetaM (Option Expr) := do
  match expr with
  | .app (.app (.const `CategoryTheory.Functor.map _) F) _ => return some F
  | .app (.app (.const `CategoryTheory.whiskerLeft _) F) _ => return some F
  | .app (.app (.const `CategoryTheory.whiskerRight _) _) G => return some G
  | _ => return none

-- Check if an expression involves functors
def involvesFunctors (expr : Expr) : MetaM Bool := do
  match ← extractFunctor expr with
  | some _ => return true
  | none => return false

-- Apply all functor and whiskering rules
def applyAllFunctorRules (expr : Expr) : MetaM Expr := do
  let step1 ← applyFunctorLemmas expr
  let step2 ← applyWhiskeringLemmas step1
  return step2

end CatNF
