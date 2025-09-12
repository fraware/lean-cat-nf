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

-- Isomorphism transport and normalization

-- Push/pull Iso.hom/Iso.inv through compositions to cancel pairs
def shuntIsomorphisms (segments : List ExprSegment) : MetaM (List ExprSegment) := do
  let mut result := segments
  let mut changed := true

  while changed do
    changed := false
    let mut newResult := []
    let mut i := 0

    while i < result.length do
      let seg := result[i]!
      match seg with
      | .iso_hom iso =>
        -- Look for matching iso_inv
        if i + 1 < result.length then
          let nextSeg := result[i + 1]!
          match nextSeg with
          | .iso_inv iso' =>
            if iso == iso' then
              -- Cancel hom ≫ inv = 𝟙
              newResult := newResult ++ [.id]
              i := i + 2
              changed := true
            else
              newResult := newResult ++ [seg]
              i := i + 1
          | _ =>
            newResult := newResult ++ [seg]
            i := i + 1
        else
          newResult := newResult ++ [seg]
          i := i + 1
      | .iso_inv iso =>
        -- Look for matching iso_hom
        if i + 1 < result.length then
          let nextSeg := result[i + 1]!
          match nextSeg with
          | .iso_hom iso' =>
            if iso == iso' then
              -- Cancel inv ≫ hom = 𝟙
              newResult := newResult ++ [.id]
              i := i + 2
              changed := true
            else
              newResult := newResult ++ [seg]
              i := i + 1
          | _ =>
            newResult := newResult ++ [seg]
            i := i + 1
        else
          newResult := newResult ++ [seg]
          i := i + 1
      | .comp f g =>
        -- Recursively process compositions
        let fSegs ← shuntIsomorphisms [f]
        let gSegs ← shuntIsomorphisms [g]
        let newComp := ExprSegment.comp fSegs.head! gSegs.head!
        newResult := newResult ++ [newComp]
        i := i + 1
      | _ =>
        newResult := newResult ++ [seg]
        i := i + 1

    result := newResult

  return result

-- Apply isomorphism cancellation rules
def applyIsoCancellation (expr : Expr) : MetaM Expr := do
  try
    simpOnly [CategoryTheory.Iso.hom_inv_id, CategoryTheory.Iso.inv_hom_id] expr
  catch _ =>
    return expr

-- Normalize isomorphism compositions
def normalizeIsoCompositions (segments : List ExprSegment) : MetaM (List ExprSegment) := do
  let mut result := segments
  let mut changed := true

  while changed do
    changed := false
    let mut newResult := []

    for i in [0:result.length] do
      let seg := result[i]!
      match seg with
      | .comp (.iso_hom iso1) (.iso_hom iso2) =>
        -- (iso1.hom) ≫ (iso2.hom) = (iso1 ≫ iso2).hom
        let newIso := ExprSegment.iso_hom (mkApp2 (mkConst `CategoryTheory.Iso.trans) iso1 iso2)
        newResult := newResult ++ [newIso]
        changed := true
      | .comp (.iso_inv iso1) (.iso_inv iso2) =>
        -- (iso1.inv) ≫ (iso2.inv) = (iso2 ≫ iso1).inv
        let newIso := ExprSegment.iso_inv (mkApp2 (mkConst `CategoryTheory.Iso.trans) iso2 iso1)
        newResult := newResult ++ [newIso]
        changed := true
      | .comp (.iso_hom iso) (.iso_inv iso') =>
        if iso == iso' then
          -- iso.hom ≫ iso.inv = 𝟙
          newResult := newResult ++ [.id]
          changed := true
        else
          newResult := newResult ++ [seg]
      | .comp (.iso_inv iso) (.iso_hom iso') =>
        if iso == iso' then
          -- iso.inv ≫ iso.hom = 𝟙
          newResult := newResult ++ [.id]
          changed := true
        else
          newResult := newResult ++ [seg]
      | _ =>
        newResult := newResult ++ [seg]

    result := newResult

  return result

-- Apply isomorphism functoriality rules
def applyIsoFunctoriality (segments : List ExprSegment) : MetaM (List ExprSegment) := do
  let mut result := segments
  let mut changed := true

  while changed do
    changed := false
    let mut newResult := []

    for i in [0:result.length] do
      let seg := result[i]!
      match seg with
      | .functor_map F (.iso_hom iso) =>
        -- F.map (iso.hom) = (F.mapIso iso).hom
        let newIso := ExprSegment.iso_hom (mkApp (mkConst `CategoryTheory.Functor.mapIso) F iso)
        newResult := newResult ++ [newIso]
        changed := true
      | .functor_map F (.iso_inv iso) =>
        -- F.map (iso.inv) = (F.mapIso iso).inv
        let newIso := ExprSegment.iso_inv (mkApp (mkConst `CategoryTheory.Functor.mapIso) F iso)
        newResult := newResult ++ [newIso]
        changed := true
      | _ =>
        newResult := newResult ++ [seg]

    result := newResult

  return result

-- Main isomorphism transport normalization
def normalizeIsoTransport (segments : List ExprSegment) : MetaM (List ExprSegment) := do
  let step1 ← shuntIsomorphisms segments
  let step2 ← normalizeIsoCompositions step1
  let step3 ← applyIsoFunctoriality step2
  return step3

-- Check if two expressions are isomorphic
def areIsomorphic (e1 e2 : Expr) : MetaM Bool := do
  let segs1 ← flattenComposition e1
  let segs2 ← flattenComposition e2
  let norm1 ← normalizeIsoTransport segs1
  let norm2 ← normalizeIsoTransport segs2
  return norm1 == norm2

-- Apply isomorphism lemmas to an expression
def applyIsoLemmas (expr : Expr) : MetaM Expr := do
  try
    simpOnly [
      CategoryTheory.Iso.hom_inv_id,
      CategoryTheory.Iso.inv_hom_id,
      CategoryTheory.Iso.trans_hom,
      CategoryTheory.Iso.trans_inv,
      CategoryTheory.Functor.mapIso
    ] expr
  catch _ =>
    return expr

-- Extract isomorphism from an expression
def extractIso (expr : Expr) : MetaM (Option Expr) := do
  match expr with
  | .app (.const `CategoryTheory.Iso.hom _) iso => return some iso
  | .app (.const `CategoryTheory.Iso.inv _) iso => return some iso
  | _ => return none

-- Check if an expression is an isomorphism
def isIso (expr : Expr) : MetaM Bool := do
  match ← extractIso expr with
  | some _ => return true
  | none => return false

-- Apply isomorphism transport rules
def applyIsoTransport (expr : Expr) : MetaM Expr := do
  try
    simpOnly [
      CategoryTheory.Iso.hom_inv_id,
      CategoryTheory.Iso.inv_hom_id,
      CategoryTheory.Iso.trans_hom,
      CategoryTheory.Iso.trans_inv,
      CategoryTheory.Iso.comp_right_hom,
      CategoryTheory.Iso.comp_left_hom,
      CategoryTheory.Iso.comp_right_inv,
      CategoryTheory.Iso.comp_left_inv
    ] expr
  catch _ =>
    return expr

end CatNF
