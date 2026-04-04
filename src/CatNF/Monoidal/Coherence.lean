import Mathlib.CategoryTheory.Category.Basic
import Mathlib.CategoryTheory.Functor.Basic
import Mathlib.CategoryTheory.Iso
import Mathlib.CategoryTheory.Monoidal.Category
import Mathlib.CategoryTheory.Monoidal.Braided.Basic
import Mathlib.Data.List.Basic
import Mathlib.Data.Array.Basic
import Lean.Expr
import Lean.Meta
import Lean.Elab.Command
import Mathlib.Tactic.Basic
import Mathlib.Tactic.SimpRw
import CatNF.Core
import CatNF.Monoidal.Core

open Lean Meta

namespace CatNF.Monoidal

def normalizeCoherencePass (segments : List ExprSegment) : MetaM (List ExprSegment) := do
  let mut result : List ExprSegment := segments
  let mut changed := true
  while changed do
    changed := false
    let mut newResult : List ExprSegment := []
    for i in List.range result.length do
      match result.get? i with
      | none => pure ()
      | some seg =>
        match seg with
        | .associator f g h =>
          let rightTensor := ExprSegment.tensor f (ExprSegment.tensor g h)
          newResult := newResult ++ [rightTensor]
          changed := true
        | .left_unitor f =>
          let newTensor := ExprSegment.tensor .id f
          newResult := newResult ++ [newTensor]
          changed := true
        | .right_unitor f =>
          let newTensor := ExprSegment.tensor f .id
          newResult := newResult ++ [newTensor]
          changed := true
        | .tensor (.tensor f g) h =>
          let newTensor := ExprSegment.tensor f (ExprSegment.tensor g h)
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

/-- Pentagon / triangle / hexagon rewrites are kept as no-ops until deep `ExprSegment` patterns are validated. -/
def applyPentagonIdentity (segments : List ExprSegment) : MetaM (List ExprSegment) :=
  return segments

def applyTriangleIdentity (segments : List ExprSegment) : MetaM (List ExprSegment) :=
  return segments

def applyHexagonIdentities (segments : List ExprSegment) : MetaM (List ExprSegment) :=
  return segments

def normalizeCoherence (segments : List ExprSegment) : MetaM (List ExprSegment) := do
  let step1 ← normalizeCoherencePass segments
  let step2 ← applyPentagonIdentity step1
  let step3 ← applyTriangleIdentity step2
  let step4 ← applyHexagonIdentities step3
  let step5 ← applySymmetry step4
  return step5

def applyCoherenceLemmas (expr : Expr) : MetaM Expr :=
  return expr

def areCoherentlyEquivalent (e1 e2 : Expr) : MetaM Bool := do
  let cfg : Config := {}
  let segs1 ← flattenCompositionM e1 cfg
  let segs2 ← flattenCompositionM e2 cfg
  let norm1 ← normalizeCoherence segs1
  let norm2 ← normalizeCoherence segs2
  return norm1 == norm2

def applyAllCoherenceRules (expr : Expr) : MetaM Expr :=
  applyCoherenceLemmas expr

end CatNF.Monoidal
