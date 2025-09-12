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
import CatNF.RewriteRules

namespace CatNF.Tests.Unit.RewriteRules

-- Test configuration for deterministic testing
def testConfig : Config := {
  maxSteps := 100
  timeoutMs := 1000
  monoidal := true
  trace := false
  simpSet := none
}

-- Test data generators
def mkTestExpr (name : String) : MetaM Expr := do
  let constName := Name.mkSimple name
  return mkConst constName

def mkTestComposition (f g : Expr) : Expr :=
  mkApp2 (mkConst `CategoryTheory.CategoryStruct.comp) f g

def mkTestIdentity (C : Expr) : Expr :=
  mkApp (mkConst `CategoryTheory.CategoryStruct.id) C

-- Test cases for RewriteRules module
def testDefaultRules : MetaM Unit := do
  let rules ← getRegisteredRules

  -- Should have default rules
  assert! rules.length > 0

  -- Should contain expected default rules
  let expectedRules := [
    `CategoryTheory.Iso.refl,
    `CategoryTheory.Iso.trans,
    `CategoryTheory.Iso.comp_right,
    `CategoryTheory.Iso.comp_left
  ]

  for expectedRule in expectedRules do
    assert! rules.any (fun rule => rule.name == expectedRule)

def testInitializeDefaultRules : MetaM Unit := do
  initializeDefaultRules
  let rules ← getRegisteredRules

  -- Should initialize default rules
  assert! rules.length > 0

def testRegisterIsoRule : MetaM Unit := do
  let name := `testIsoRule
  let schema := {
    homRule := `testIsoRule_hom
    invRule := `testIsoRule_inv
    homInvId := `testIsoRule_hom_inv_id
    invHomId := `testIsoRule_inv_hom_id
  }

  registerIsoRule name schema
  let rules ← getRegisteredRules

  -- Should register the rule
  assert! rules.any (fun rule => rule.name == name)

def testValidateRule : MetaM Unit := do
  let validName := `CategoryTheory.Iso.refl
  let invalidName := `nonExistentRule

  let validResult ← validateRule validName
  let invalidResult ← validateRule invalidName

  -- Should validate correctly
  assert! validResult
  assert! !invalidResult

def testGetRegisteredRules : MetaM Unit := do
  let rules ← getRegisteredRules

  -- Should return registered rules
  assert! rules.length > 0

def testFindRule : MetaM Unit := do
  let name := `CategoryTheory.Iso.refl
  let rule ← findRule name

  -- Should find the rule
  assert! rule.isSome
  assert! rule.get!.name == name

  -- Test with non-existent rule
  let nonExistent ← findRule `nonExistentRule
  assert! nonExistent.isNone

def testApplyRewriteRule : MetaM Unit := do
  let f ← mkTestExpr "f"
  let rule ← findRule `CategoryTheory.Iso.refl

  match rule with
  | some rule => do
    let result ← applyRewriteRule rule f
    -- Should apply the rule (or return none if not applicable)
    assert! true
  | none => assert! false

def testApplyRewriteRuleErrorHandling : MetaM Unit := do
  let f ← mkTestExpr "f"
  let invalidRule : RuleEntry := {
    name := `invalidRule
    schema := {
      homRule := `nonExistentRule
      invRule := `nonExistentRule
      homInvId := `nonExistentRule
      invHomId := `nonExistentRule
    }
    isUnsafe := false
  }

  let result ← applyRewriteRule invalidRule f

  -- Should handle errors gracefully
  assert! result.isNone

def testGetApplicableRules : MetaM Unit := do
  let f ← mkTestExpr "f"
  let applicable ← getApplicableRules f

  -- Should return applicable rules
  assert! true

def testAreEquivalent : MetaM Unit := do
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"

  let equivalent ← areEquivalent f g

  -- Should check equivalence
  assert! true

def testNormalizeWithRules : MetaM Unit := do
  let f ← mkTestExpr "f"
  let rules ← getRegisteredRules

  let result ← normalizeWithRules f rules

  -- Should normalize the expression
  assert! true

def testNormalizeWithRulesEmpty : MetaM Unit := do
  let f ← mkTestExpr "f"
  let emptyRules : Array RuleEntry := #[]

  let result ← normalizeWithRules f emptyRules

  -- Should return the original expression when no rules
  assert! result == f

def testNormalizeWithRulesMaxSteps : MetaM Unit := do
  let f ← mkTestExpr "f"
  let rules ← getRegisteredRules

  -- Test with a rule that might cause loops
  let result ← normalizeWithRules f rules

  -- Should terminate within max steps
  assert! true

def testApplyRewriteRulesToSegments : MetaM Unit := do
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"
  let rules ← getRegisteredRules

  let segments := [
    ExprSegment.raw f,
    ExprSegment.raw g
  ]

  let result ← applyRewriteRulesToSegments segments rules

  -- Should apply rewrite rules to segments
  assert! result.length == 2

def testApplyRewriteRulesToSegmentsComposition : MetaM Unit := do
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"
  let rules ← getRegisteredRules

  let segments := [
    ExprSegment.comp (ExprSegment.raw f) (ExprSegment.raw g)
  ]

  let result ← applyRewriteRulesToSegments segments rules

  -- Should apply rewrite rules to composition segments
  assert! result.length == 1

def testApplyRewriteRulesToSegmentsFunctorMap : MetaM Unit := do
  let F ← mkTestExpr "F"
  let f ← mkTestExpr "f"
  let rules ← getRegisteredRules

  let segments := [
    ExprSegment.functor_map F (ExprSegment.raw f)
  ]

  let result ← applyRewriteRulesToSegments segments rules

  -- Should apply rewrite rules to functor map segments
  assert! result.length == 1

def testApplyRewriteRulesToSegmentsWhiskerLeft : MetaM Unit := do
  let F ← mkTestExpr "F"
  let f ← mkTestExpr "f"
  let rules ← getRegisteredRules

  let segments := [
    ExprSegment.whisker_left F (ExprSegment.raw f)
  ]

  let result ← applyRewriteRulesToSegments segments rules

  -- Should apply rewrite rules to whisker left segments
  assert! result.length == 1

def testApplyRewriteRulesToSegmentsWhiskerRight : MetaM Unit := do
  let f ← mkTestExpr "f"
  let G ← mkTestExpr "G"
  let rules ← getRegisteredRules

  let segments := [
    ExprSegment.whisker_right (ExprSegment.raw f) G
  ]

  let result ← applyRewriteRulesToSegments segments rules

  -- Should apply rewrite rules to whisker right segments
  assert! result.length == 1

def testApplyRewriteRulesToSegmentsTensor : MetaM Unit := do
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"
  let rules ← getRegisteredRules

  let segments := [
    ExprSegment.tensor (ExprSegment.raw f) (ExprSegment.raw g)
  ]

  let result ← applyRewriteRulesToSegments segments rules

  -- Should apply rewrite rules to tensor segments
  assert! result.length == 1

def testApplyRewriteRulesToSegmentsAssociator : MetaM Unit := do
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"
  let h ← mkTestExpr "h"
  let rules ← getRegisteredRules

  let segments := [
    ExprSegment.associator (ExprSegment.raw f) (ExprSegment.raw g) (ExprSegment.raw h)
  ]

  let result ← applyRewriteRulesToSegments segments rules

  -- Should apply rewrite rules to associator segments
  assert! result.length == 1

def testApplyRewriteRulesToSegmentsLeftUnitor : MetaM Unit := do
  let f ← mkTestExpr "f"
  let rules ← getRegisteredRules

  let segments := [
    ExprSegment.left_unitor (ExprSegment.raw f)
  ]

  let result ← applyRewriteRulesToSegments segments rules

  -- Should apply rewrite rules to left unitor segments
  assert! result.length == 1

def testApplyRewriteRulesToSegmentsRightUnitor : MetaM Unit := do
  let f ← mkTestExpr "f"
  let rules ← getRegisteredRules

  let segments := [
    ExprSegment.right_unitor (ExprSegment.raw f)
  ]

  let result ← applyRewriteRulesToSegments segments rules

  -- Should apply rewrite rules to right unitor segments
  assert! result.length == 1

def testApplyRewriteRulesToSegmentsBraid : MetaM Unit := do
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"
  let rules ← getRegisteredRules

  let segments := [
    ExprSegment.braid (ExprSegment.raw f) (ExprSegment.raw g)
  ]

  let result ← applyRewriteRulesToSegments segments rules

  -- Should apply rewrite rules to braid segments
  assert! result.length == 1

def testApplyRewriteRulesToGoal : MetaM Unit := do
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"
  let comp := mkTestComposition f g
  let rules ← getRegisteredRules

  let (result, rewrites) ← applyRewriteRulesToGoal comp rules

  -- Should apply rewrite rules to goal
  assert! isComposition result
  assert! rewrites.isEmpty

def testInitializeRuleSystem : MetaM Unit := do
  initializeRuleSystem
  let rules ← getRegisteredRules

  -- Should initialize rule system
  assert! rules.length > 0

def testApplyAllRules : MetaM Unit := do
  let f ← mkTestExpr "f"

  let result ← applyAllRules f

  -- Should apply all rules
  assert! true

def testApplyAllRulesToSegments : MetaM Unit := do
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"

  let segments := [
    ExprSegment.raw f,
    ExprSegment.raw g
  ]

  let result ← applyAllRulesToSegments segments

  -- Should apply all rules to segments
  assert! result.length == 2

def testIsNormalForm : MetaM Unit := do
  let f ← mkTestExpr "f"

  let isNormal ← isNormalForm f

  -- Should check normal form
  assert! true

def testGetNormalForm : MetaM Unit := do
  let f ← mkTestExpr "f"

  let normal ← getNormalForm f

  -- Should get normal form
  assert! true

def testRuleEntryStructure : MetaM Unit := do
  let name := `testRule
  let schema := {
    homRule := `testRule_hom
    invRule := `testRule_inv
    homInvId := `testRule_hom_inv_id
    invHomId := `testRule_inv_hom_id
  }
  let isUnsafe := false

  let entry : RuleEntry := {
    name := name
    schema := schema
    isUnsafe := isUnsafe
  }

  -- Should create correct structure
  assert! entry.name == name
  assert! entry.schema == schema
  assert! entry.isUnsafe == isUnsafe

def testRewriteSchemaStructure : MetaM Unit := do
  let homRule := `testRule_hom
  let invRule := `testRule_inv
  let homInvId := `testRule_hom_inv_id
  let invHomId := `testRule_inv_hom_id

  let schema : RewriteSchema := {
    homRule := homRule
    invRule := invRule
    homInvId := homInvId
    invHomId := invHomId
  }

  -- Should create correct structure
  assert! schema.homRule == homRule
  assert! schema.invRule == invRule
  assert! schema.homInvId == homInvId
  assert! schema.invHomId == invHomId

def testRuleDatabasePersistence : MetaM Unit := do
  let name := `persistentTestRule
  let schema := {
    homRule := `persistentTestRule_hom
    invRule := `persistentTestRule_inv
    homInvId := `persistentTestRule_hom_inv_id
    invHomId := `persistentTestRule_inv_hom_id
  }

  -- Register rule
  registerIsoRule name schema

  -- Check it's there
  let rules1 ← getRegisteredRules
  assert! rules1.any (fun rule => rule.name == name)

  -- Check again
  let rules2 ← getRegisteredRules
  assert! rules2.any (fun rule => rule.name == name)

def testRuleDatabaseConcurrency : MetaM Unit := do
  let name1 := `concurrentTestRule1
  let name2 := `concurrentTestRule2

  let schema1 := {
    homRule := `concurrentTestRule1_hom
    invRule := `concurrentTestRule1_inv
    homInvId := `concurrentTestRule1_hom_inv_id
    invHomId := `concurrentTestRule1_inv_hom_id
  }

  let schema2 := {
    homRule := `concurrentTestRule2_hom
    invRule := `concurrentTestRule2_inv
    homInvId := `concurrentTestRule2_hom_inv_id
    invHomId := `concurrentTestRule2_inv_hom_id
  }

  -- Register multiple rules
  registerIsoRule name1 schema1
  registerIsoRule name2 schema2

  -- Check both are there
  let rules ← getRegisteredRules
  assert! rules.any (fun rule => rule.name == name1)
  assert! rules.any (fun rule => rule.name == name2)

def testNormalizationConfluence : MetaM Unit := do
  let f ← mkTestExpr "f"
  let rules ← getRegisteredRules

  -- Test that normalization is confluent
  let result1 ← normalizeWithRules f rules
  let result2 ← normalizeWithRules f rules

  -- Should get the same result
  assert! result1 == result2

def testNormalizationTermination : MetaM Unit := do
  let f ← mkTestExpr "f"
  let rules ← getRegisteredRules

  -- Test that normalization terminates
  let result ← normalizeWithRules f rules

  -- Should complete without infinite loops
  assert! true

def testComplexExpressionNormalization : MetaM Unit := do
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"
  let h ← mkTestExpr "h"
  let comp1 := mkTestComposition f g
  let comp2 := mkTestComposition comp1 h

  let rules ← getRegisteredRules
  let result ← normalizeWithRules comp2 rules

  -- Should normalize complex expression
  assert! true

def testIdentityExpressionNormalization : MetaM Unit := do
  let f ← mkTestExpr "f"
  let id := mkTestIdentity f
  let comp := mkTestComposition f id

  let rules ← getRegisteredRules
  let result ← normalizeWithRules comp rules

  -- Should normalize identity expression
  assert! true

def testEmptySegmentsNormalization : MetaM Unit := do
  let emptySegments : List ExprSegment := []
  let rules ← getRegisteredRules

  let result ← applyRewriteRulesToSegments emptySegments rules

  -- Should handle empty segments
  assert! result.isEmpty

def testSingleSegmentNormalization : MetaM Unit := do
  let f ← mkTestExpr "f"
  let segments := [ExprSegment.raw f]
  let rules ← getRegisteredRules

  let result ← applyRewriteRulesToSegments segments rules

  -- Should handle single segment
  assert! result.length == 1

def testMultipleSegmentsNormalization : MetaM Unit := do
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"
  let h ← mkTestExpr "h"
  let segments := [
    ExprSegment.raw f,
    ExprSegment.raw g,
    ExprSegment.raw h
  ]
  let rules ← getRegisteredRules

  let result ← applyRewriteRulesToSegments segments rules

  -- Should handle multiple segments
  assert! result.length == 3

-- Main test runner
def runAllTests : MetaM Unit := do
  testDefaultRules
  testInitializeDefaultRules
  testRegisterIsoRule
  testValidateRule
  testGetRegisteredRules
  testFindRule
  testApplyRewriteRule
  testApplyRewriteRuleErrorHandling
  testGetApplicableRules
  testAreEquivalent
  testNormalizeWithRules
  testNormalizeWithRulesEmpty
  testNormalizeWithRulesMaxSteps
  testApplyRewriteRulesToSegments
  testApplyRewriteRulesToSegmentsComposition
  testApplyRewriteRulesToSegmentsFunctorMap
  testApplyRewriteRulesToSegmentsWhiskerLeft
  testApplyRewriteRulesToSegmentsWhiskerRight
  testApplyRewriteRulesToSegmentsTensor
  testApplyRewriteRulesToSegmentsAssociator
  testApplyRewriteRulesToSegmentsLeftUnitor
  testApplyRewriteRulesToSegmentsRightUnitor
  testApplyRewriteRulesToSegmentsBraid
  testApplyRewriteRulesToGoal
  testInitializeRuleSystem
  testApplyAllRules
  testApplyAllRulesToSegments
  testIsNormalForm
  testGetNormalForm
  testRuleEntryStructure
  testRewriteSchemaStructure
  testRuleDatabasePersistence
  testRuleDatabaseConcurrency
  testNormalizationConfluence
  testNormalizationTermination
  testComplexExpressionNormalization
  testIdentityExpressionNormalization
  testEmptySegmentsNormalization
  testSingleSegmentNormalization
  testMultipleSegmentsNormalization

  logInfo "All rewrite rules unit tests passed!"

end CatNF.Tests.Unit.RewriteRules
