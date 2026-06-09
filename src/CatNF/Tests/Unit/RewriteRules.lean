import Mathlib.CategoryTheory.Category.Basic
import Mathlib.CategoryTheory.Functor.Basic
import Mathlib.CategoryTheory.Iso
import Mathlib.CategoryTheory.Monoidal.Category
import Mathlib.CategoryTheory.Whiskering
import Lean.Expr
import Lean.Meta
import Lean.Elab.Command
import CatNF.Core
import CatNF.RewriteRules

namespace CatNF.Tests.Unit.RewriteRules

open Lean Meta CatNF

def testConfig : Config := {
  maxSteps := 100
  timeoutMs := 1000
  monoidal := true
  trace := false
  simpSet := none
}

def mkTestExpr (name : String) : MetaM Expr := do
  return mkConst (Name.mkSimple name)

def testDefaultRules : MetaM Unit := do
  let rules ← runCatNFM! getRegisteredRules
  assert! rules.size > 0
  assert! rules.any (fun r => r.name == `CategoryTheory.Iso.refl)

def testRegisterAndFind : MetaM Unit := do
  let name := `customTestRule
  let schema : RewriteSchema := {
    homRule := `customTestRule_hom
    invRule := `customTestRule_inv
    homInvId := `customTestRule_hom_inv_id
    invHomId := `customTestRule_inv_hom_id
  }
  runCatNFM! (registerIsoRule name schema)
  let rules ← runCatNFM! getRegisteredRules
  assert! rules.any (fun r => r.name == name)
  let found ← runCatNFM! (findRule name)
  match found with
  | some r => assert! r.schema == schema
  | none => assert! false

def testApplyRewriteRuleStub : MetaM Unit := do
  let f ← mkTestExpr "f"
  let r ← runCatNFM! (findRule `CategoryTheory.Iso.refl)
  match r with
  | some re =>
    let out ← runCatNFM! (applyRewriteRule re f)
    assert! out.isNone
  | none => assert! false

def testNormalizeWithRulesIdentity : MetaM Unit := do
  let f ← mkTestExpr "f"
  let rules ← runCatNFM! getRegisteredRules
  let out ← runCatNFM! (normalizeWithRules f rules)
  assert! out == f

def testAreEquivalent : MetaM Unit := do
  let f ← mkTestExpr "f"
  let ok ← runCatNFM! (areEquivalent f f)
  assert! ok

def testValidateRuleConsistency : MetaM Unit := do
  let ok ← runCatNFM! validateRuleConsistency
  assert! ok

def runAllTests : MetaM Unit := do
  runCatNFM! resetBuiltinRules
  testDefaultRules
  testRegisterAndFind
  testApplyRewriteRuleStub
  testNormalizeWithRulesIdentity
  testAreEquivalent
  testValidateRuleConsistency
  logInfo "RewriteRules unit tests passed."

end CatNF.Tests.Unit.RewriteRules
