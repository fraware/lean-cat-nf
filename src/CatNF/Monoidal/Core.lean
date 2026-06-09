import Mathlib.CategoryTheory.Monoidal.Category
import Lean.Meta
import CatNF.Core.Segments
import CatNF.Core.Config
import CatNF.Core.Normalize

open Lean Meta

namespace CatNF.Monoidal

def applyAssociators (segments : List ExprSegment) : MetaM (List ExprSegment) := do
  let mut result : List ExprSegment := segments
  let mut changed := true
  while changed do
    changed := false
    let mut newResult : List ExprSegment := []
    for i in List.range result.length do
      match result[i]? with
      | none => pure ()
      | some seg =>
        match seg with
        | .associator f g h =>
          let rightTensor := ExprSegment.tensor f (ExprSegment.tensor g h)
          newResult := newResult ++ [rightTensor]
          changed := true
        | .tensor (.tensor f g) h =>
          let newTensor := ExprSegment.tensor f (ExprSegment.tensor g h)
          newResult := newResult ++ [newTensor]
          changed := true
        | _ =>
          newResult := newResult ++ [seg]
    result := newResult
  return result

def applyUnitors (segments : List ExprSegment) : MetaM (List ExprSegment) := do
  let mut result : List ExprSegment := segments
  let mut changed := true
  while changed do
    changed := false
    let mut newResult : List ExprSegment := []
    for i in List.range result.length do
      match result[i]? with
      | none => pure ()
      | some seg =>
        match seg with
        | .left_unitor f =>
          let newTensor := ExprSegment.tensor .id f
          newResult := newResult ++ [newTensor]
          changed := true
        | .right_unitor f =>
          let newTensor := ExprSegment.tensor f .id
          newResult := newResult ++ [newTensor]
          changed := true
        | .tensor .id g =>
          newResult := newResult ++ [g]
          changed := true
        | .tensor f .id =>
          newResult := newResult ++ [f]
          changed := true
        | _ =>
          newResult := newResult ++ [seg]
    result := newResult
  return result

def applyBraiding (segments : List ExprSegment) : MetaM (List ExprSegment) := do
  let mut result : List ExprSegment := segments
  let mut changed := true
  while changed do
    changed := false
    let mut newResult : List ExprSegment := []
    for i in List.range result.length do
      match result[i]? with
      | none => pure ()
      | some seg =>
        match seg with
        | .braid f g =>
          let newTensor := ExprSegment.tensor g f
          newResult := newResult ++ [newTensor]
          changed := true
        | _ =>
          newResult := newResult ++ [seg]
    result := newResult
  return result

def applySymmetry (segments : List ExprSegment) : MetaM (List ExprSegment) := do
  let mut result : List ExprSegment := segments
  let mut changed := true
  while changed do
    changed := false
    let mut newResult : List ExprSegment := []
    for i in List.range result.length do
      match result[i]? with
      | none => pure ()
      | some seg =>
        match seg with
        | .braid f g =>
          let newTensor := ExprSegment.tensor f g
          newResult := newResult ++ [newTensor]
          changed := true
        | _ =>
          newResult := newResult ++ [seg]
    result := newResult
  return result

def normalizeMonoidal (segments : List ExprSegment) : MetaM (List ExprSegment) := do
  let step1 ← applyAssociators segments
  let step2 ← applyUnitors step1
  let step3 ← applyBraiding step2
  let step4 ← applySymmetry step3
  return step4

def applyMonoidalLemmas (expr : Expr) : MetaM Expr :=
  return expr

def applyBraidingLemmas (expr : Expr) : MetaM Expr :=
  return expr

def areMonoidallyEquivalent (e1 e2 : Expr) : MetaM Bool := do
  let cfg : Config := {}
  let segs1 ← flattenCompositionM e1 cfg
  let segs2 ← flattenCompositionM e2 cfg
  let norm1 ← normalizeMonoidal segs1
  let norm2 ← normalizeMonoidal segs2
  return norm1 == norm2

def extractMonoidalStructure (expr : Expr) : MetaM (Option (Expr × Expr)) := do
  match expr with
  | .app (.app (.const `CategoryTheory.MonoidalCategory.tensorObj _) f) g => return some (f, g)
  | _ => return none

def involvesMonoidalStructure (expr : Expr) : MetaM Bool := do
  match ← extractMonoidalStructure expr with
  | some _ => return true
  | none => return false

def applyAllMonoidalRules (expr : Expr) : MetaM Expr := do
  let step1 ← applyMonoidalLemmas expr
  applyBraidingLemmas step1

end CatNF.Monoidal
