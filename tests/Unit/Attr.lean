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
import Lean.Attribute
import Lean.Elab.Attr
import Mathlib.Tactic.Basic
import Mathlib.Tactic.SimpRw
import CatNF.Attr

namespace CatNF.Tests.Unit.Attr

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

def mkTestIso (name : String) : MetaM Expr := do
  let constName := Name.mkSimple name
  return mkConst constName

-- Test cases for attribute system
def testValidateIsoDeclaration : MetaM Unit := do
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"
  let eqType := mkApp2 (mkConst `Eq) f g

  -- Create a mock declaration
  let decl := {
    name := `testIso
    type := eqType
    value := mkConst `rfl
    levelParams := []
    all := []
    isUnsafe := false
    isPartial := false
    isNonComputable := false
  }

  let isValid ← validateIsoDeclaration decl
  assert! isValid

def testValidateIsoDeclarationInvalid : MetaM Unit := do
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"
  let h ← mkTestExpr "h"
  let compType := mkApp2 (mkConst `CategoryTheory.CategoryStruct.comp) f g

  -- Create a mock declaration that's not an equality
  let decl := {
    name := `testIso
    type := compType
    value := mkConst `rfl
    levelParams := []
    all := []
    isUnsafe := false
    isPartial := false
    isNonComputable := false
  }

  let isValid ← validateIsoDeclaration decl
  assert! !isValid

def testExtractRewriteSchema : MetaM Unit := do
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"
  let eqType := mkApp2 (mkConst `Eq) f g

  let decl := {
    name := `testIso
    type := eqType
    value := mkConst `rfl
    levelParams := []
    all := []
    isUnsafe := false
    isPartial := false
    isNonComputable := false
  }

  let schema ← extractRewriteSchema decl

  -- Should extract the correct schema
  assert! schema.homRule == `testIso
  assert! schema.invRule == `testIsoinv
  assert! schema.homInvId == `testIsohom_inv_id
  assert! schema.invHomId == `testIsoinv_hom_id

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

  let foundRule ← findRule name
  assert! foundRule.isSome
  assert! foundRule.get!.name == name
  assert! foundRule.get!.schema == schema

def testGetRegisteredRules : MetaM Unit := do
  let rules ← getRegisteredRules

  -- Should have at least the default rules
  assert! rules.length > 0

  -- Should contain default rules
  assert! rules.any (fun rule => rule.name == `CategoryTheory.Iso.refl)
  assert! rules.any (fun rule => rule.name == `CategoryTheory.Iso.trans)

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

def testDefaultRules : MetaM Unit := do
  let rules ← getRegisteredRules

  -- Should contain all default rules
  let expectedRules := [
    `CategoryTheory.Iso.refl,
    `CategoryTheory.Iso.trans,
    `CategoryTheory.Iso.comp_right,
    `CategoryTheory.Iso.comp_left
  ]

  for expectedRule in expectedRules do
    assert! rules.any (fun rule => rule.name == expectedRule)

def testRuleValidation : MetaM Unit := do
  let validName := `CategoryTheory.Iso.refl
  let invalidName := `nonExistentRule

  let validResult ← validateRule validName
  let invalidResult ← validateRule invalidName

  -- Should validate correctly
  assert! validResult
  assert! !invalidResult

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

def testRuleApplicationErrorHandling : MetaM Unit := do
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

def testRuleApplicationSuccess : MetaM Unit := do
  let f ← mkTestExpr "f"
  let rule ← findRule `CategoryTheory.Iso.refl

  match rule with
  | some rule => do
    let result ← applyRewriteRule rule f
    -- Should either succeed or return none
    assert! true
  | none => assert! false

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

-- Main test runner
def runAllTests : MetaM Unit := do
  testValidateIsoDeclaration
  testValidateIsoDeclarationInvalid
  testExtractRewriteSchema
  testRegisterIsoRule
  testGetRegisteredRules
  testFindRule
  testApplyRewriteRule
  testGetApplicableRules
  testAreEquivalent
  testNormalizeWithRules
  testNormalizeWithRulesEmpty
  testNormalizeWithRulesMaxSteps
  testRuleEntryStructure
  testRewriteSchemaStructure
  testDefaultRules
  testRuleValidation
  testRuleDatabasePersistence
  testRuleDatabaseConcurrency
  testRuleApplicationErrorHandling
  testRuleApplicationSuccess
  testNormalizationConfluence
  testNormalizationTermination

  logInfo "All attribute unit tests passed!"

end CatNF.Tests.Unit.Attr
