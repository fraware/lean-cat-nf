import Mathlib.CategoryTheory.Category.Basic
import Mathlib.CategoryTheory.Functor.Basic
import Mathlib.CategoryTheory.Iso
import Lean.Expr
import Lean.Meta
import Lean.Elab.Tactic
import CatNF.Core
import CatNF.Tactic
import CatNF.Attr
import CatNF.RewriteRules

namespace CatNF.ValidationTests

-- ============================================================================
-- VALIDATION TESTS
-- ============================================================================

/-- Test valid configuration validation -/
def testValidConfig : MetaM Unit := do
  logInfo "Testing valid configuration validation..."

  -- Test default configuration
  let config1 : Config := { maxSteps := 500, timeoutMs := 1500, monoidal := true, trace := false }
  let _ ← validateConfig config1
  logInfo "Default configuration validation passed"

  -- Test custom configuration
  let config2 : Config := { maxSteps := 1000, timeoutMs := 3000, monoidal := false, trace := true, simpSet := some "custom" }
  let _ ← validateConfig config2
  logInfo "Custom configuration validation passed"

  -- Test edge case configuration
  let config3 : Config := { maxSteps := 10000, timeoutMs := 30000, monoidal := true, trace := false }
  let _ ← validateConfig config3
  logInfo "Edge case configuration validation passed"

/-- Test valid expression validation -/
def testValidExpressionValidation : MetaM Unit := do
  logInfo "Testing valid expression validation..."

  let config : Config := { maxSteps := 500, timeoutMs := 1500, monoidal := true, trace := false }

  -- Test valid expressions
  let idExpr := mkConst `CategoryTheory.CategoryStruct.id
  let compExpr := mkApp2 (mkConst `CategoryTheory.CategoryStruct.comp) idExpr idExpr
  let isoExpr := mkApp (mkConst `CategoryTheory.Iso.hom) idExpr

  -- Test flattenComposition with valid expressions
  let _ ← flattenComposition idExpr config
  logInfo "Identity expression validation passed"

  let _ ← flattenComposition compExpr config
  logInfo "Composition expression validation passed"

  let _ ← flattenComposition isoExpr config
  logInfo "Isomorphism expression validation passed"

/-- Test valid segment validation -/
def testValidSegmentValidation : MetaM Unit := do
  logInfo "Testing valid segment validation..."

  -- Test valid segments
  let idSeg := ExprSegment.id
  let compSeg := ExprSegment.comp ExprSegment.id ExprSegment.id
  let isoHomSeg := ExprSegment.iso_hom (mkConst `CategoryTheory.Iso.refl)
  let isoInvSeg := ExprSegment.iso_inv (mkConst `CategoryTheory.Iso.refl)
  let functorMapSeg := ExprSegment.functor_map (mkConst `CategoryTheory.Functor.id) ExprSegment.id
  let whiskerLeftSeg := ExprSegment.whisker_left (mkConst `CategoryTheory.Functor.id) ExprSegment.id
  let whiskerRightSeg := ExprSegment.whisker_right ExprSegment.id (mkConst `CategoryTheory.Functor.id)
  let tensorSeg := ExprSegment.tensor ExprSegment.id ExprSegment.id
  let associatorSeg := ExprSegment.associator ExprSegment.id ExprSegment.id ExprSegment.id
  let leftUnitorSeg := ExprSegment.left_unitor ExprSegment.id
  let rightUnitorSeg := ExprSegment.right_unitor ExprSegment.id
  let braidSeg := ExprSegment.braid ExprSegment.id ExprSegment.id
  let rawSeg := ExprSegment.raw (mkConst `CategoryTheory.CategoryStruct.id)

  -- Test individual segment validation
  let _ ← validateExprSegment idSeg
  logInfo "Identity segment validation passed"

  let _ ← validateExprSegment compSeg
  logInfo "Composition segment validation passed"

  let _ ← validateExprSegment isoHomSeg
  logInfo "Isomorphism hom segment validation passed"

  let _ ← validateExprSegment isoInvSeg
  logInfo "Isomorphism inv segment validation passed"

  let _ ← validateExprSegment functorMapSeg
  logInfo "Functor map segment validation passed"

  let _ ← validateExprSegment whiskerLeftSeg
  logInfo "Whisker left segment validation passed"

  let _ ← validateExprSegment whiskerRightSeg
  logInfo "Whisker right segment validation passed"

  let _ ← validateExprSegment tensorSeg
  logInfo "Tensor segment validation passed"

  let _ ← validateExprSegment associatorSeg
  logInfo "Associator segment validation passed"

  let _ ← validateExprSegment leftUnitorSeg
  logInfo "Left unitor segment validation passed"

  let _ ← validateExprSegment rightUnitorSeg
  logInfo "Right unitor segment validation passed"

  let _ ← validateExprSegment braidSeg
  logInfo "Braid segment validation passed"

  let _ ← validateExprSegment rawSeg
  logInfo "Raw segment validation passed"

  -- Test segment list validation
  let segments := [idSeg, compSeg, isoHomSeg, isoInvSeg, functorMapSeg]
  let _ ← validateExprSegments segments
  logInfo "Segment list validation passed"

/-- Test valid rule validation -/
def testValidRuleValidation : MetaM Unit := do
  logInfo "Testing valid rule validation..."

  -- Test valid rewrite schema
  let schema : RewriteSchema := ⟨`test, `test, `test, `test⟩
  let _ ← validateRewriteSchema schema
  logInfo "Valid rewrite schema validation passed"

  -- Test valid rule entry
  let rule : RuleEntry := ⟨`test, schema, false, 0, "Test rule"⟩
  let _ ← validateRuleEntry rule
  logInfo "Valid rule entry validation passed"

  -- Test rule with priority
  let priorityRule : RuleEntry := ⟨`test, schema, false, 100, "Priority rule"⟩
  let _ ← validateRuleEntry priorityRule
  logInfo "Priority rule validation passed"

  -- Test unsafe rule
  let unsafeRule : RuleEntry := ⟨`test, schema, true, 0, "Unsafe rule"⟩
  let _ ← validateRuleEntry unsafeRule
  logInfo "Unsafe rule validation passed"

/-- Test valid monoidal validation -/
def testValidMonoidalValidation : MetaM Unit := do
  logInfo "Testing valid monoidal validation..."

  -- Test valid tensor word
  let tensorWord := TensorWord.atom (mkConst `CategoryTheory.CategoryStruct.id)
  let _ ← validateTensorWord tensorWord
  logInfo "Valid tensor word validation passed"

  -- Test valid coherence cache
  let cache : CoherenceCache := { maxSize := 1000 }
  let _ ← validateCoherenceCache cache
  logInfo "Valid coherence cache validation passed"

  -- Test coherence cache with entries
  let cacheWithEntries : CoherenceCache := {
    cache := #[(tensorWord, mkConst `CategoryTheory.CategoryStruct.id)],
    maxSize := 1000
  }
  let _ ← validateCoherenceCache cacheWithEntries
  logInfo "Valid coherence cache with entries validation passed"

/-- Test valid applied rewrite validation -/
def testValidAppliedRewriteValidation : MetaM Unit := do
  logInfo "Testing valid applied rewrite validation..."

  -- Test valid applied rewrite
  let rewrite : AppliedRewrite := ⟨"test_rule", mkConst `CategoryTheory.CategoryStruct.id, mkConst `CategoryTheory.CategoryStruct.id, 1⟩
  let _ ← validateAppliedRewrite rewrite
  logInfo "Valid applied rewrite validation passed"

  -- Test applied rewrite with timestamp
  let rewriteWithTimestamp : AppliedRewrite := ⟨"test_rule", mkConst `CategoryTheory.CategoryStruct.id, mkConst `CategoryTheory.CategoryStruct.id, 1, 12345⟩
  let _ ← validateAppliedRewrite rewriteWithTimestamp
  logInfo "Valid applied rewrite with timestamp validation passed"

/-- Test valid normal form state validation -/
def testValidNormalFormStateValidation : MetaM Unit := do
  logInfo "Testing valid normal form state validation..."

  let config : Config := { maxSteps := 500, timeoutMs := 1500, monoidal := true, trace := false }
  let segments := [ExprSegment.id, ExprSegment.id]
  let rewrites := [⟨"test_rule", mkConst `CategoryTheory.CategoryStruct.id, mkConst `CategoryTheory.CategoryStruct.id, 1⟩]

  -- Test valid normal form state
  let state : NFState := ⟨segments, rewrites, 0, config, 0⟩
  let _ ← validateNFState state
  logInfo "Valid normal form state validation passed"

  -- Test normal form state with steps
  let stateWithSteps : NFState := ⟨segments, rewrites, 100, config, 0⟩
  let _ ← validateNFState stateWithSteps
  logInfo "Valid normal form state with steps validation passed"

/-- Test comprehensive validation -/
def testComprehensiveValidation : MetaM Unit := do
  logInfo "Testing comprehensive validation..."

  -- Test valid configuration
  testValidConfig

  -- Test valid expression validation
  testValidExpressionValidation

  -- Test valid segment validation
  testValidSegmentValidation

  -- Test valid rule validation
  testValidRuleValidation

  -- Test valid monoidal validation
  testValidMonoidalValidation

  -- Test valid applied rewrite validation
  testValidAppliedRewriteValidation

  -- Test valid normal form state validation
  testValidNormalFormStateValidation

  logInfo "All validation tests completed successfully!"

/-- Test edge case validation -/
def testEdgeCaseValidation : MetaM Unit := do
  logInfo "Testing edge case validation..."

  -- Test maximum allowed values
  let maxConfig : Config := { maxSteps := 10000, timeoutMs := 30000, monoidal := true, trace := false }
  let _ ← validateConfig maxConfig
  logInfo "Maximum configuration validation passed"

  -- Test maximum segment list
  let maxSegments := List.replicate 1000 ExprSegment.id
  let _ ← validateExprSegments maxSegments
  logInfo "Maximum segment list validation passed"

  -- Test maximum rule description
  let maxDesc := String.mk (List.replicate 500 'a')
  let maxRule : RuleEntry := ⟨`test, ⟨`test, `test, `test, `test⟩, false, 1000, maxDesc⟩
  let _ ← validateRuleEntry maxRule
  logInfo "Maximum rule description validation passed"

  -- Test maximum coherence cache
  let maxCache : CoherenceCache := { maxSize := 10000 }
  let _ ← validateCoherenceCache maxCache
  logInfo "Maximum coherence cache validation passed"

  logInfo "All edge case validation tests completed successfully!"

end CatNF.ValidationTests
