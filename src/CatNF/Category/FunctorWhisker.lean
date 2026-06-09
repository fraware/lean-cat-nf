import Mathlib.CategoryTheory.Whiskering
import Lean.Meta
import CatNF.Core.Segments
import CatNF.Core.Config
import CatNF.Core.Normalize
import CatNF.Core.MorphismNames

open Lean Meta
open CatNF.MorphismNames

namespace CatNF

def flattenMapComp (segments : List ExprSegment) : MetaM (List ExprSegment) := do
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
        | .functor_map F (.comp f g) =>
          let fMap := ExprSegment.functor_map F f
          let gMap := ExprSegment.functor_map F g
          newResult := newResult ++ [fMap, gMap]
          changed := true
        | .functor_map _ .id =>
          newResult := newResult ++ [.id]
          changed := true
        | _ =>
          newResult := newResult ++ [seg]
    result := newResult
  return result

def standardizeWhiskering (segments : List ExprSegment) : MetaM (List ExprSegment) := do
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
        | .whisker_left F (.comp f g) =>
          let fWhisker := ExprSegment.whisker_left F f
          let gWhisker := ExprSegment.whisker_left F g
          newResult := newResult ++ [fWhisker, gWhisker]
          changed := true
        | .whisker_right (.comp f g) G =>
          let fWhisker := ExprSegment.whisker_right f G
          let gWhisker := ExprSegment.whisker_right g G
          newResult := newResult ++ [fWhisker, gWhisker]
          changed := true
        | .whisker_left _ .id =>
          newResult := newResult ++ [.id]
          changed := true
        | .whisker_right .id _ =>
          newResult := newResult ++ [.id]
          changed := true
        | _ =>
          newResult := newResult ++ [seg]
    result := newResult
  return result

def applyFunctoriality (segments : List ExprSegment) : MetaM (List ExprSegment) := do
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
        | .comp a b =>
          match a, b with
          | .functor_map F f, .functor_map F' g =>
            if F == F' then
              let newMap := ExprSegment.functor_map F (ExprSegment.comp f g)
              newResult := newResult ++ [newMap]
              changed := true
            else
              newResult := newResult ++ [seg]
          | .whisker_left F f, .whisker_left F' g =>
            if F == F' then
              let newWhisker := ExprSegment.whisker_left F (ExprSegment.comp f g)
              newResult := newResult ++ [newWhisker]
              changed := true
            else
              newResult := newResult ++ [seg]
          | .whisker_right f G, .whisker_right g' G' =>
            if G == G' then
              let newWhisker := ExprSegment.whisker_right (ExprSegment.comp f g') G
              newResult := newResult ++ [newWhisker]
              changed := true
            else
              newResult := newResult ++ [seg]
          | _, _ =>
            newResult := newResult ++ [seg]
        | _ =>
          newResult := newResult ++ [seg]
    result := newResult
  return result

def applyWhiskeringCommutation (segments : List ExprSegment) : MetaM (List ExprSegment) := do
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
        | .comp wl wr =>
          match wl, wr with
          | .whisker_left F f, .whisker_right g G =>
            let newComp := ExprSegment.comp (.whisker_right g G) (.whisker_left F f)
            newResult := newResult ++ [newComp]
            changed := true
          | .whisker_right f G, .whisker_left F g =>
            let newComp := ExprSegment.comp (.whisker_left F g) (.whisker_right f G)
            newResult := newResult ++ [newComp]
            changed := true
          | _, _ =>
            newResult := newResult ++ [seg]
        | _ =>
          newResult := newResult ++ [seg]
    result := newResult
  return result

def normalizeFunctorWhisker (segments : List ExprSegment) : MetaM (List ExprSegment) := do
  let step1 ← flattenMapComp segments
  let step2 ← standardizeWhiskering step1
  let step3 ← applyFunctoriality step2
  let step4 ← applyWhiskeringCommutation step3
  return step4

def applyFunctorLemmas (expr : Expr) : MetaM Expr :=
  return expr

def applyWhiskeringLemmas (expr : Expr) : MetaM Expr :=
  return expr

def areFunctoriallyEquivalent (e1 e2 : Expr) : MetaM Bool := do
  let cfg : Config := {}
  let segs1 ← flattenCompositionM e1 cfg
  let segs2 ← flattenCompositionM e2 cfg
  let norm1 ← normalizeFunctorWhisker segs1
  let norm2 ← normalizeFunctorWhisker segs2
  return norm1 == norm2

def extractFunctor (expr : Expr) : MetaM (Option Expr) := do
  if let some (F, _) := asFunctorMap? expr then
    return some F
  if let some (F, _) := asWhiskerLeft? expr then
    return some F
  if let some (_, G) := asWhiskerRight? expr then
    return some G
  return none

def involvesFunctors (expr : Expr) : MetaM Bool := do
  match ← extractFunctor expr with
  | some _ => return true
  | none => return false

def applyAllFunctorRules (expr : Expr) : MetaM Expr := do
  let step1 ← applyFunctorLemmas expr
  applyWhiskeringLemmas step1

end CatNF
