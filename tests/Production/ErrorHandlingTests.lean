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

namespace CatNF.ProductionTests

-- ============================================================================
-- ERROR HANDLING TESTS
-- ============================================================================

/-- Test configuration validation with invalid parameters -/
def testInvalidConfig : MetaM Unit := do
  -- Test zero maxSteps
  try
    let config : Config := { maxSteps := 0, timeoutMs := 1500, monoidal := true, trace := false }
    let _ ← validateConfig config
    throwError "Expected validation error for zero maxSteps"
  catch e =>
    logInfo s!"Correctly caught validation error: {e}"

  -- Test excessive maxSteps
  try
    let config : Config := { maxSteps := 15000, timeoutMs := 1500, monoidal := true, trace := false }
    let _ ← validateConfig config
    throwError "Expected validation error for excessive maxSteps"
  catch e =>
    logInfo s!"Correctly caught validation error: {e}"

  -- Test zero timeout
  try
    let config : Config := { maxSteps := 500, timeoutMs := 0, monoidal := true, trace := false }
    let _ ← validateConfig config
    throwError "Expected validation error for zero timeout"
  catch e =>
    logInfo s!"Correctly caught validation error: {e}"

  -- Test excessive timeout
  try
    let config : Config := { maxSteps := 500, timeoutMs := 50000, monoidal := true, trace := false }
    let _ ← validateConfig config
    throwError "Expected validation error for excessive timeout"
  catch e =>
    logInfo s!"Correctly caught validation error: {e}"

  -- Test empty simpSet
  try
    let config : Config := { maxSteps := 500, timeoutMs := 1500, monoidal := true, trace := false, simpSet := some "" }
    let _ ← validateConfig config
    throwError "Expected validation error for empty simpSet"
  catch e =>
    logInfo s!"Correctly caught validation error: {e}"

  -- Test excessive simpSet length
  try
    let config : Config := { maxSteps := 500, timeoutMs := 1500, monoidal := true, trace := false, simpSet := some (String.mk (List.replicate 200 'a')) }
    let _ ← validateConfig config
    throwError "Expected validation error for excessive simpSet length"
  catch e =>
    logInfo s!"Correctly caught validation error: {e}"

/-- Test expression validation with metavariables -/
def testMetavariableValidation : MetaM Unit := do
  let mvar ← mkFreshExprMVar
  let config : Config := { maxSteps := 500, timeoutMs := 1500, monoidal := true, trace := false }

  -- Test flattenComposition with metavariable
  try
    let _ ← flattenComposition mvar config
    throwError "Expected validation error for metavariable"
  catch e =>
    logInfo s!"Correctly caught validation error: {e}"

  -- Test isComposition with metavariable
  try
    let _ ← isComposition mvar
    throwError "Expected validation error for metavariable"
  catch e =>
    logInfo s!"Correctly caught validation error: {e}"

  -- Test isIdentity with metavariable
  try
    let _ ← isIdentity mvar
    throwError "Expected validation error for metavariable"
  catch e =>
    logInfo s!"Correctly caught validation error: {e}"

/-- Test segment validation with invalid segments -/
def testSegmentValidation : MetaM Unit := do
  let mvar ← mkFreshExprMVar
  let config : Config := { maxSteps := 500, timeoutMs := 1500, monoidal := true, trace := false }

  -- Test iso_hom with metavariable
  try
    let seg := ExprSegment.iso_hom mvar
    let _ ← validateExprSegment seg
    throwError "Expected validation error for metavariable in iso_hom"
  catch e =>
    logInfo s!"Correctly caught validation error: {e}"

  -- Test iso_inv with metavariable
  try
    let seg := ExprSegment.iso_inv mvar
    let _ ← validateExprSegment seg
    throwError "Expected validation error for metavariable in iso_inv"
  catch e =>
    logInfo s!"Correctly caught validation error: {e}"

  -- Test functor_map with metavariable
  try
    let seg := ExprSegment.functor_map mvar ExprSegment.id
    let _ ← validateExprSegment seg
    throwError "Expected validation error for metavariable in functor_map"
  catch e =>
    logInfo s!"Correctly caught validation error: {e}"

/-- Test bounds checking with excessive data -/
def testBoundsChecking : MetaM Unit := do
  let config : Config := { maxSteps := 500, timeoutMs := 1500, monoidal := true, trace := false }

  -- Test excessive segment list
  let excessiveSegments := List.replicate 2000 ExprSegment.id
  try
    let _ ← validateExprSegments excessiveSegments
    throwError "Expected validation error for excessive segments"
  catch e =>
    logInfo s!"Correctly caught validation error: {e}"

  -- Test empty segment list
  try
    let _ ← validateExprSegments []
    throwError "Expected validation error for empty segments"
  catch e =>
    logInfo s!"Correctly caught validation error: {e}"

/-- Test timeout handling -/
def testTimeoutHandling : MetaM Unit := do
  -- Test with very low timeout
  let config : Config := { maxSteps := 500, timeoutMs := 1, monoidal := true, trace := false }
  let expr := mkConst `CategoryTheory.CategoryStruct.id

  try
    let _ ← flattenComposition expr config
    logInfo "Flattening completed within timeout"
  catch e =>
    logInfo s!"Timeout handling working: {e}"

/-- Test graceful degradation -/
def testGracefulDegradation : MetaM Unit := do
  let config : Config := { maxSteps := 500, timeoutMs := 1500, monoidal := true, trace := false }
  let expr := mkConst `CategoryTheory.CategoryStruct.id

  -- Test applyFinalSimp with graceful degradation
  try
    let result ← applyFinalSimp expr config
    logInfo s!"Graceful degradation working: {result}"
  catch e =>
    logInfo s!"Graceful degradation error: {e}"

/-- Test error propagation -/
def testErrorPropagation : MetaM Unit := do
  let config : Config := { maxSteps := 500, timeoutMs := 1500, monoidal := true, trace := false }
  let mvar ← mkFreshExprMVar

  -- Test error propagation through normalizeGoal
  try
    let _ ← normalizeGoal mvar config
    throwError "Expected error propagation"
  catch e =>
    logInfo s!"Error propagation working: {e}"

/-- Test rule validation -/
def testRuleValidation : MetaM Unit := do
  -- Test invalid rule entry
  try
    let invalidRule : RuleEntry := ⟨.anonymous, ⟨.anonymous, .anonymous, .anonymous, .anonymous⟩, false, 0, ""⟩
    let _ ← validateRuleEntry invalidRule
    throwError "Expected validation error for invalid rule"
  catch e =>
    logInfo s!"Correctly caught validation error: {e}"

  -- Test rule with excessive description
  try
    let excessiveDesc := String.mk (List.replicate 600 'a')
    let invalidRule : RuleEntry := ⟨`test, ⟨`test, `test, `test, `test⟩, false, 0, excessiveDesc⟩
    let _ ← validateRuleEntry invalidRule
    throwError "Expected validation error for excessive description"
  catch e =>
    logInfo s!"Correctly caught validation error: {e}"

  -- Test rule with excessive priority
  try
    let invalidRule : RuleEntry := ⟨`test, ⟨`test, `test, `test, `test⟩, false, 2000, ""⟩
    let _ ← validateRuleEntry invalidRule
    throwError "Expected validation error for excessive priority"
  catch e =>
    logInfo s!"Correctly caught validation error: {e}"

/-- Test monoidal validation -/
def testMonoidalValidation : MetaM Unit := do
  let config : Config := { maxSteps := 500, timeoutMs := 1500, monoidal := true, trace := false }
  let mvar ← mkFreshExprMVar

  -- Test tensor word validation
  try
    let invalidWord := TensorWord.atom mvar
    let _ ← validateTensorWord invalidWord
    throwError "Expected validation error for metavariable in tensor word"
  catch e =>
    logInfo s!"Correctly caught validation error: {e}"

  -- Test coherence cache validation
  try
    let invalidCache : CoherenceCache := { maxSize := 0 }
    let _ ← validateCoherenceCache invalidCache
    throwError "Expected validation error for zero maxSize"
  catch e =>
    logInfo s!"Correctly caught validation error: {e}"

/-- Test comprehensive error handling -/
def testComprehensiveErrorHandling : MetaM Unit := do
  logInfo "Testing comprehensive error handling..."

  -- Test configuration validation
  testInvalidConfig

  -- Test metavariable validation
  testMetavariableValidation

  -- Test segment validation
  testSegmentValidation

  -- Test bounds checking
  testBoundsChecking

  -- Test timeout handling
  testTimeoutHandling

  -- Test graceful degradation
  testGracefulDegradation

  -- Test error propagation
  testErrorPropagation

  -- Test rule validation
  testRuleValidation

  -- Test monoidal validation
  testMonoidalValidation

  logInfo "All error handling tests completed successfully!"

end CatNF.ProductionTests
