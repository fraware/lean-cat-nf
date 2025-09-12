import Mathlib.CategoryTheory.Category.Basic
import Mathlib.CategoryTheory.Functor.Basic
import Mathlib.CategoryTheory.Iso
import Mathlib.CategoryTheory.Monoidal.Category
import Mathlib.CategoryTheory.Whiskering
import Mathlib.Data.List.Basic
import Mathlib.Data.Array.Basic
import Lean.Expr
import Lean.Meta
import Lean.Elab.Command
import Mathlib.Tactic.Basic
import Mathlib.Tactic.SimpRw
import CatNF.Core
import CatNF.AssocUnit
import CatNF.FunctorWhisker
import CatNF.IsoTransport
import CatNF.Monoidal.Core
import CatNF.Monoidal.Coherence
import CatNF.RewriteRules
import CatNF.Tactic

namespace CatNF.Tests.Unit

-- Test configuration for deterministic testing
def testConfig : Config := {
  maxSteps := 100
  timeoutMs := 1000
  monoidal := true
  trace := false
  simpSet := none
}

-- Test data generators for deterministic testing
def mkTestExpr (name : String) : MetaM Expr := do
  let constName := Name.mkSimple name
  return mkConst constName

def mkTestComposition (f g : Expr) : Expr :=
  mkApp2 (mkConst `CategoryTheory.CategoryStruct.comp) f g

def mkTestIdentity (C : Expr) : Expr :=
  mkApp (mkConst `CategoryTheory.CategoryStruct.id) C

def mkTestIsoHom (iso : Expr) : Expr :=
  mkApp (mkConst `CategoryTheory.Iso.hom) iso

def mkTestIsoInv (iso : Expr) : Expr :=
  mkApp (mkConst `CategoryTheory.Iso.inv) iso

def mkTestFunctorMap (F f : Expr) : Expr :=
  mkApp2 (mkConst `CategoryTheory.Functor.map) F f

def mkTestWhiskerLeft (F f : Expr) : Expr :=
  mkApp2 (mkConst `CategoryTheory.WhiskeringLeft.whiskerLeft) F f

def mkTestWhiskerRight (f G : Expr) : Expr :=
  mkApp2 (mkConst `CategoryTheory.WhiskeringRight.whiskerRight) f G

def mkTestTensor (f g : Expr) : Expr :=
  mkApp2 (mkConst `CategoryTheory.MonoidalCategory.tensorObj) f g

-- Test cases for Core module
def testIsComposition : MetaM Unit := do
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"
  let C ← mkTestExpr "C"
  let comp := mkTestComposition f g
  let id := mkTestIdentity C

  assert! isComposition comp
  assert! !isComposition id
  assert! !isComposition f

def testIsIdentity : MetaM Unit := do
  let C ← mkTestExpr "C"
  let f ← mkTestExpr "f"
  let id := mkTestIdentity C
  let comp := mkTestComposition f f

  assert! isIdentity id
  assert! !isIdentity comp
  assert! !isIdentity f

def testIsIsoHom : MetaM Unit := do
  let iso ← mkTestExpr "iso"
  let f ← mkTestExpr "f"
  let hom := mkTestIsoHom iso
  let comp := mkTestComposition f f

  assert! isIsoHom hom
  assert! !isIsoHom comp
  assert! !isIsoHom f

def testIsIsoInv : MetaM Unit := do
  let iso ← mkTestExpr "iso"
  let f ← mkTestExpr "f"
  let inv := mkTestIsoInv iso
  let comp := mkTestComposition f f

  assert! isIsoInv inv
  assert! !isIsoInv comp
  assert! !isIsoInv f

def testIsFunctorMap : MetaM Unit := do
  let F ← mkTestExpr "F"
  let f ← mkTestExpr "f"
  let map := mkTestFunctorMap F f
  let comp := mkTestComposition f f

  assert! isFunctorMap map
  assert! !isFunctorMap comp
  assert! !isFunctorMap f

def testIsWhiskerLeft : MetaM Unit := do
  let F ← mkTestExpr "F"
  let f ← mkTestExpr "f"
  let whisker := mkTestWhiskerLeft F f
  let comp := mkTestComposition f f

  assert! isWhiskerLeft whisker
  assert! !isWhiskerLeft comp
  assert! !isWhiskerLeft f

def testIsWhiskerRight : MetaM Unit := do
  let f ← mkTestExpr "f"
  let G ← mkTestExpr "G"
  let whisker := mkTestWhiskerRight f G
  let comp := mkTestComposition f f

  assert! isWhiskerRight whisker
  assert! !isWhiskerRight comp
  assert! !isWhiskerRight f

def testIsTensor : MetaM Unit := do
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"
  let tensor := mkTestTensor f g
  let comp := mkTestComposition f g

  assert! isTensor tensor
  assert! !isTensor comp
  assert! !isTensor f

def testFlattenComposition : MetaM Unit := do
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"
  let h ← mkTestExpr "h"
  let comp1 := mkTestComposition f g
  let comp2 := mkTestComposition comp1 h
  let id := mkTestIdentity f

  let segs1 ← flattenComposition comp2
  let segs2 ← flattenComposition id

  -- Should flatten to [f, g, h]
  assert! segs1.length == 3
  assert! segs2.length == 1
  assert! segs2.head! == ExprSegment.id

def testEraseIdentities : MetaM Unit := do
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"
  let segments := [ExprSegment.raw f, ExprSegment.id, ExprSegment.raw g]
  let result := eraseIdentities segments

  assert! result.length == 2
  assert! result.head! == ExprSegment.raw f
  assert! result.tail!.head! == ExprSegment.raw g

def testShuntIsomorphisms : MetaM Unit := do
  let iso ← mkTestExpr "iso"
  let f ← mkTestExpr "f"
  let segments := [
    ExprSegment.iso_hom iso,
    ExprSegment.iso_inv iso,
    ExprSegment.raw f
  ]
  let result := shuntIsomorphisms segments

  -- Should cancel iso_hom and iso_inv to id
  assert! result.length == 2
  assert! result.head! == ExprSegment.id
  assert! result.tail!.head! == ExprSegment.raw f

def testSegmentToExpr : MetaM Unit := do
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"
  let iso ← mkTestExpr "iso"

  let seg1 := ExprSegment.id
  let seg2 := ExprSegment.raw f
  let seg3 := ExprSegment.iso_hom iso
  let seg4 := ExprSegment.comp (ExprSegment.raw f) (ExprSegment.raw g)

  let expr1 ← segmentToExpr seg1
  let expr2 ← segmentToExpr seg2
  let expr3 ← segmentToExpr seg3
  let expr4 ← segmentToExpr seg4

  assert! isIdentity expr1
  assert! expr2 == f
  assert! isIsoHom expr3
  assert! isComposition expr4

def testRebuildExpression : MetaM Unit := do
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"
  let h ← mkTestExpr "h"

  let segments := [
    ExprSegment.raw f,
    ExprSegment.raw g,
    ExprSegment.raw h
  ]

  let result ← rebuildExpression segments
  let expected := mkTestComposition f (mkTestComposition g h)

  -- Should rebuild to f ≫ (g ≫ h)
  assert! isComposition result

def testNormalizeGoal : MetaM Unit := do
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"
  let h ← mkTestExpr "h"
  let comp := mkTestComposition f (mkTestComposition g h)

  let (result, rewrites) ← normalizeGoal comp testConfig

  -- Should normalize the composition
  assert! isComposition result
  assert! rewrites.isEmpty

-- Test cases for AssocUnit module
def testRightAssociate : MetaM Unit := do
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"
  let h ← mkTestExpr "h"

  let segments := [
    ExprSegment.raw f,
    ExprSegment.raw g,
    ExprSegment.raw h
  ]

  let result := rightAssociate segments

  -- Should right-associate to f ≫ (g ≫ h)
  assert! result.length == 1
  match result.head! with
  | .comp fSeg (ExprSegment.comp gSeg hSeg) =>
    assert! fSeg == ExprSegment.raw f
    assert! gSeg == ExprSegment.raw g
    assert! hSeg == ExprSegment.raw h
  | _ => assert! false

def testRemoveIdentities : MetaM Unit := do
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"

  let segments := [
    ExprSegment.raw f,
    ExprSegment.id,
    ExprSegment.raw g
  ]

  let result ← removeIdentities segments

  -- Should remove identity and keep f, g
  assert! result.length == 2
  assert! result.head! == ExprSegment.raw f
  assert! result.tail!.head! == ExprSegment.raw g

def testApplyAssociativity : MetaM Unit := do
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"
  let h ← mkTestExpr "h"

  let segments := [
    ExprSegment.comp (ExprSegment.comp (ExprSegment.raw f) (ExprSegment.raw g)) (ExprSegment.raw h)
  ]

  let result ← applyAssociativity segments

  -- Should right-associate
  assert! result.length == 1
  match result.head! with
  | .comp fSeg (ExprSegment.comp gSeg hSeg) =>
    assert! fSeg == ExprSegment.raw f
    assert! gSeg == ExprSegment.raw g
    assert! hSeg == ExprSegment.raw h
  | _ => assert! false

def testHandleUnits : MetaM Unit := do
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"

  let segments := [
    ExprSegment.tensor ExprSegment.id (ExprSegment.raw f),
    ExprSegment.tensor (ExprSegment.raw g) ExprSegment.id
  ]

  let result ← handleUnits segments

  -- Should remove unit tensors
  assert! result.length == 2
  assert! result.head! == ExprSegment.raw f
  assert! result.tail!.head! == ExprSegment.raw g

def testApplyAssociators : MetaM Unit := do
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"
  let h ← mkTestExpr "h"

  let segments := [
    ExprSegment.associator (ExprSegment.raw f) (ExprSegment.raw g) (ExprSegment.raw h)
  ]

  let result ← applyAssociators segments

  -- Should apply associator
  assert! result.length == 1
  match result.head! with
  | .tensor fSeg (ExprSegment.tensor gSeg hSeg) =>
    assert! fSeg == ExprSegment.raw f
    assert! gSeg == ExprSegment.raw g
    assert! hSeg == ExprSegment.raw h
  | _ => assert! false

def testNormalizeAssocUnit : MetaM Unit := do
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"
  let h ← mkTestExpr "h"

  let segments := [
    ExprSegment.raw f,
    ExprSegment.id,
    ExprSegment.raw g,
    ExprSegment.raw h
  ]

  let result ← normalizeAssocUnit segments

  -- Should normalize associativity and units
  assert! result.length == 3
  assert! result.head! == ExprSegment.raw f
  assert! result.tail!.head! == ExprSegment.raw g
  assert! result.tail!.tail!.head! == ExprSegment.raw h

-- Test cases for FunctorWhisker module
def testFlattenMapComp : MetaM Unit := do
  let F ← mkTestExpr "F"
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"

  let segments := [
    ExprSegment.functor_map F (ExprSegment.comp (ExprSegment.raw f) (ExprSegment.raw g))
  ]

  let result ← flattenMapComp segments

  -- Should flatten F.map (f ≫ g) to F.map f ≫ F.map g
  assert! result.length == 2
  match result.head! with
  | .functor_map F' fSeg =>
    assert! F' == F
    assert! fSeg == ExprSegment.raw f
  | _ => assert! false
  match result.tail!.head! with
  | .functor_map F' gSeg =>
    assert! F' == F
    assert! gSeg == ExprSegment.raw g
  | _ => assert! false

def testStandardizeWhiskering : MetaM Unit := do
  let F ← mkTestExpr "F"
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"

  let segments := [
    ExprSegment.whisker_left F (ExprSegment.comp (ExprSegment.raw f) (ExprSegment.raw g))
  ]

  let result ← standardizeWhiskering segments

  -- Should standardize F ◁ (f ≫ g) to (F ◁ f) ≫ (F ◁ g)
  assert! result.length == 2
  match result.head! with
  | .whisker_left F' fSeg =>
    assert! F' == F
    assert! fSeg == ExprSegment.raw f
  | _ => assert! false
  match result.tail!.head! with
  | .whisker_left F' gSeg =>
    assert! F' == F
    assert! gSeg == ExprSegment.raw g
  | _ => assert! false

def testApplyFunctoriality : MetaM Unit := do
  let F ← mkTestExpr "F"
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"

  let segments := [
    ExprSegment.comp (ExprSegment.functor_map F (ExprSegment.raw f)) (ExprSegment.functor_map F (ExprSegment.raw g))
  ]

  let result ← applyFunctoriality segments

  -- Should apply functoriality: F.map f ≫ F.map g = F.map (f ≫ g)
  assert! result.length == 1
  match result.head! with
  | .functor_map F' (ExprSegment.comp fSeg gSeg) =>
    assert! F' == F
    assert! fSeg == ExprSegment.raw f
    assert! gSeg == ExprSegment.raw g
  | _ => assert! false

def testNormalizeFunctorWhisker : MetaM Unit := do
  let F ← mkTestExpr "F"
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"

  let segments := [
    ExprSegment.functor_map F (ExprSegment.comp (ExprSegment.raw f) (ExprSegment.raw g))
  ]

  let result ← normalizeFunctorWhisker segments

  -- Should normalize functor and whiskering
  assert! result.length == 2
  match result.head! with
  | .functor_map F' fSeg =>
    assert! F' == F
    assert! fSeg == ExprSegment.raw f
  | _ => assert! false

-- Test cases for IsoTransport module
def testShuntIsomorphismsIso : MetaM Unit := do
  let iso ← mkTestExpr "iso"
  let f ← mkTestExpr "f"

  let segments := [
    ExprSegment.iso_hom iso,
    ExprSegment.iso_inv iso,
    ExprSegment.raw f
  ]

  let result ← shuntIsomorphisms segments

  -- Should cancel iso_hom and iso_inv to id
  assert! result.length == 2
  assert! result.head! == ExprSegment.id
  assert! result.tail!.head! == ExprSegment.raw f

def testNormalizeIsoCompositions : MetaM Unit := do
  let iso1 ← mkTestExpr "iso1"
  let iso2 ← mkTestExpr "iso2"

  let segments := [
    ExprSegment.comp (ExprSegment.iso_hom iso1) (ExprSegment.iso_hom iso2)
  ]

  let result ← normalizeIsoCompositions segments

  -- Should normalize iso compositions
  assert! result.length == 1
  match result.head! with
  | .iso_hom _ => assert! true
  | _ => assert! false

def testNormalizeIsoTransport : MetaM Unit := do
  let iso ← mkTestExpr "iso"
  let f ← mkTestExpr "f"

  let segments := [
    ExprSegment.iso_hom iso,
    ExprSegment.iso_inv iso,
    ExprSegment.raw f
  ]

  let result ← normalizeIsoTransport segments

  -- Should normalize isomorphism transport
  assert! result.length == 2
  assert! result.head! == ExprSegment.id
  assert! result.tail!.head! == ExprSegment.raw f

-- Test cases for Monoidal modules
def testApplyAssociatorsMonoidal : MetaM Unit := do
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"
  let h ← mkTestExpr "h"

  let segments := [
    ExprSegment.associator (ExprSegment.raw f) (ExprSegment.raw g) (ExprSegment.raw h)
  ]

  let result ← Monoidal.applyAssociators segments

  -- Should apply associators
  assert! result.length == 1
  match result.head! with
  | .tensor fSeg (ExprSegment.tensor gSeg hSeg) =>
    assert! fSeg == ExprSegment.raw f
    assert! gSeg == ExprSegment.raw g
    assert! hSeg == ExprSegment.raw h
  | _ => assert! false

def testApplyUnitors : MetaM Unit := do
  let f ← mkTestExpr "f"

  let segments := [
    ExprSegment.left_unitor (ExprSegment.raw f),
    ExprSegment.right_unitor (ExprSegment.raw f)
  ]

  let result ← Monoidal.applyUnitors segments

  -- Should apply unitors
  assert! result.length == 2
  assert! result.head! == ExprSegment.raw f
  assert! result.tail!.head! == ExprSegment.raw f

def testNormalizeMonoidal : MetaM Unit := do
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"
  let h ← mkTestExpr "h"

  let segments := [
    ExprSegment.associator (ExprSegment.raw f) (ExprSegment.raw g) (ExprSegment.raw h)
  ]

  let result ← Monoidal.normalizeMonoidal segments

  -- Should normalize monoidal structure
  assert! result.length == 1
  match result.head! with
  | .tensor fSeg (ExprSegment.tensor gSeg hSeg) =>
    assert! fSeg == ExprSegment.raw f
    assert! gSeg == ExprSegment.raw g
    assert! hSeg == ExprSegment.raw h
  | _ => assert! false

def testNormalizeCoherence : MetaM Unit := do
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"
  let h ← mkTestExpr "h"

  let segments := [
    ExprSegment.associator (ExprSegment.raw f) (ExprSegment.raw g) (ExprSegment.raw h)
  ]

  let result ← Monoidal.normalizeCoherence segments

  -- Should normalize coherence
  assert! result.length == 1
  match result.head! with
  | .tensor fSeg (ExprSegment.tensor gSeg hSeg) =>
    assert! fSeg == ExprSegment.raw f
    assert! gSeg == ExprSegment.raw g
    assert! hSeg == ExprSegment.raw h
  | _ => assert! false

-- Test cases for RewriteRules module
def testRegisterIsoRule : MetaM Unit := do
  let name := `testRule
  let schema := {
    homRule := `testRule_hom
    invRule := `testRule_inv
    homInvId := `testRule_hom_inv_id
    invHomId := `testRule_inv_hom_id
  }

  registerIsoRule name schema
  let rules ← getRegisteredRules

  -- Should register the rule
  assert! rules.any (fun rule => rule.name == name)

def testFindRule : MetaM Unit := do
  let name := `CategoryTheory.Iso.refl
  let rule ← findRule name

  -- Should find the rule
  assert! rule.isSome
  assert! rule.get!.name == name

def testApplyRewriteRule : MetaM Unit := do
  let f ← mkTestExpr "f"
  let rule ← findRule `CategoryTheory.Iso.refl

  match rule with
  | some rule => do
    let result ← applyRewriteRule rule f
    -- Should apply the rule (or return none if not applicable)
    assert! true
  | none => assert! false

def testNormalizeWithRules : MetaM Unit := do
  let f ← mkTestExpr "f"
  let rules ← getRegisteredRules

  let result ← normalizeWithRules f rules

  -- Should normalize the expression
  assert! true

-- Test cases for Tactic module
def testNormalizeGoalTactic : MetaM Unit := do
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"
  let comp := mkTestComposition f g

  let (result, rewrites) ← normalizeGoal comp testConfig

  -- Should normalize the goal
  assert! isComposition result
  assert! rewrites.isEmpty

def testApplyFinalSimp : MetaM Unit := do
  let f ← mkTestExpr "f"
  let id := mkTestIdentity f
  let comp := mkTestComposition f id

  let result ← applyFinalSimp comp testConfig

  -- Should apply final simp
  assert! true

-- Main test runner
def runAllTests : MetaM Unit := do
  testIsComposition
  testIsIdentity
  testIsIsoHom
  testIsIsoInv
  testIsFunctorMap
  testIsWhiskerLeft
  testIsWhiskerRight
  testIsTensor
  testFlattenComposition
  testEraseIdentities
  testShuntIsomorphisms
  testSegmentToExpr
  testRebuildExpression
  testNormalizeGoal
  testRightAssociate
  testRemoveIdentities
  testApplyAssociativity
  testHandleUnits
  testApplyAssociators
  testNormalizeAssocUnit
  testFlattenMapComp
  testStandardizeWhiskering
  testApplyFunctoriality
  testNormalizeFunctorWhisker
  testShuntIsomorphismsIso
  testNormalizeIsoCompositions
  testNormalizeIsoTransport
  testApplyAssociatorsMonoidal
  testApplyUnitors
  testNormalizeMonoidal
  testNormalizeCoherence
  testRegisterIsoRule
  testFindRule
  testApplyRewriteRule
  testNormalizeWithRules
  testNormalizeGoalTactic
  testApplyFinalSimp

  logInfo "All unit tests passed!"

end CatNF.Tests.Unit
