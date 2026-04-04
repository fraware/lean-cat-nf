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

open Lean Meta

namespace CatNF

def shuntIsoSegmentsMeta (segments : List ExprSegment) : MetaM (List ExprSegment) :=
  shuntIsomorphisms segments

def applyIsoCancellation (expr : Expr) : MetaM Expr :=
  return expr

def normalizeIsoCompositions (segments : List ExprSegment) : MetaM (List ExprSegment) := do
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
        | .comp (.iso_hom iso1) (.iso_hom iso2) =>
          let newIso := ExprSegment.iso_hom (mkApp2 (mkConst `CategoryTheory.Iso.trans) iso1 iso2)
          newResult := newResult ++ [newIso]
          changed := true
        | .comp (.iso_inv iso1) (.iso_inv iso2) =>
          let newIso := ExprSegment.iso_inv (mkApp2 (mkConst `CategoryTheory.Iso.trans) iso2 iso1)
          newResult := newResult ++ [newIso]
          changed := true
        | .comp (.iso_hom iso) (.iso_inv iso') =>
          if iso == iso' then
            newResult := newResult ++ [.id]
            changed := true
          else
            newResult := newResult ++ [seg]
        | .comp (.iso_inv iso) (.iso_hom iso') =>
          if iso == iso' then
            newResult := newResult ++ [.id]
            changed := true
          else
            newResult := newResult ++ [seg]
        | _ =>
          newResult := newResult ++ [seg]
    result := newResult
  return result

def applyIsoFunctoriality (segments : List ExprSegment) : MetaM (List ExprSegment) := do
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
        | .functor_map F (.iso_hom iso) =>
          let newIso := ExprSegment.iso_hom (mkApp2 (mkConst `CategoryTheory.Functor.mapIso) F iso)
          newResult := newResult ++ [newIso]
          changed := true
        | .functor_map F (.iso_inv iso) =>
          let newIso := ExprSegment.iso_inv (mkApp2 (mkConst `CategoryTheory.Functor.mapIso) F iso)
          newResult := newResult ++ [newIso]
          changed := true
        | _ =>
          newResult := newResult ++ [seg]
    result := newResult
  return result

def normalizeIsoTransport (segments : List ExprSegment) : MetaM (List ExprSegment) := do
  let step1 ← shuntIsoSegmentsMeta segments
  let step2 ← normalizeIsoCompositions step1
  let step3 ← applyIsoFunctoriality step2
  return step3

def areIsomorphic (e1 e2 : Expr) : MetaM Bool := do
  let cfg : Config := {}
  let segs1 ← flattenCompositionM e1 cfg
  let segs2 ← flattenCompositionM e2 cfg
  let norm1 ← normalizeIsoTransport segs1
  let norm2 ← normalizeIsoTransport segs2
  return norm1 == norm2

def applyIsoLemmas (expr : Expr) : MetaM Expr :=
  return expr

def extractIso (expr : Expr) : MetaM (Option Expr) := do
  match expr with
  | .app (.const `CategoryTheory.Iso.hom _) iso => return some iso
  | .app (.const `CategoryTheory.Iso.inv _) iso => return some iso
  | _ => return none

def isIso (expr : Expr) : MetaM Bool := do
  match ← extractIso expr with
  | some _ => return true
  | none => return false

def applyIsoTransport (expr : Expr) : MetaM Expr :=
  return expr

end CatNF
