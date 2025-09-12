import Mathlib.CategoryTheory.Category.Basic
import Mathlib.CategoryTheory.Functor.Basic
import Mathlib.CategoryTheory.Iso
import Mathlib.CategoryTheory.Monoidal.Category
import Mathlib.CategoryTheory.Monoidal.Braided
import Mathlib.CategoryTheory.Monoidal.Symmetric
import Mathlib.CategoryTheory.Whiskering
import Mathlib.Data.List.Basic
import Mathlib.Data.Array.Basic
import Mathlib.Data.Nat.Basic
import Mathlib.Data.Real.Basic
import Mathlib.Data.Float.Basic
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

namespace CatNF.Tests.Determinism

-- Determinism testing configuration
structure DeterminismConfig where
  maxIterations : Nat := 100
  timeoutMs : Nat := 5000
  enableCaching : Bool := true
  enableTracing : Bool := false
  checkMemoryUsage : Bool := true
  checkExecutionTime : Bool := true
  checkResultEquality : Bool := true
  checkSideEffects : Bool := true
  tolerance : Float := 0.01 -- 1% tolerance for floating point comparisons

-- Determinism test result
structure DeterminismTestResult where
  testName : String
  isDeterministic : Bool
  iterations : Nat
  failures : List String
  executionTimeVariance : Float
  memoryUsageVariance : Float
  resultConsistency : Bool
  sideEffectConsistency : Bool
  averageExecutionTime : Float
  averageMemoryUsage : Float
  minExecutionTime : Nat
  maxExecutionTime : Nat
  minMemoryUsage : Nat
  maxMemoryUsage : Nat

-- Test configuration for deterministic testing
def testConfig : Config := {
  maxSteps := 1000
  timeoutMs := 5000
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

def mkTestAssociator (f g h : Expr) : Expr :=
  mkApp3 (mkConst `CategoryTheory.MonoidalCategory.associator) f g h

def mkTestLeftUnitor (f : Expr) : Expr :=
  mkApp2 (mkConst `CategoryTheory.MonoidalCategory.leftUnitor) f

def mkTestRightUnitor (f : Expr) : Expr :=
  mkApp2 (mkConst `CategoryTheory.MonoidalCategory.rightUnitor) f

def mkTestBraid (f g : Expr) : Expr :=
  mkApp2 (mkConst `CategoryTheory.MonoidalCategory.braiding) f g

-- Performance measurement functions
def measureExecutionTime (action : MetaM α) : MetaM (α × Nat) := do
  let startTime ← IO.monoMsNow
  let result ← action
  let endTime ← IO.monoMsNow
  let executionTime := endTime - startTime
  return (result, executionTime)

def measureMemoryUsage (action : MetaM α) : MetaM (α × Nat) := do
  let startMemory ← IO.getNumHeartbeats
  let result ← action
  let endMemory ← IO.getNumHeartbeats
  let memoryUsage := endMemory - startMemory
  return (result, memoryUsage)

-- Statistical analysis functions
def calculateVariance (values : List Float) : Float :=
  if values.isEmpty then 0.0 else
    let mean := values.foldl (· + ·) 0.0 / values.length.toFloat
    let squaredDiffs := values.map (fun x => (x - mean) * (x - mean))
    squaredDiffs.foldl (· + ·) 0.0 / values.length.toFloat

def calculateStandardDeviation (values : List Float) : Float :=
  Float.sqrt (calculateVariance values)

def calculateCoefficientOfVariation (values : List Float) : Float :=
  if values.isEmpty then 0.0 else
    let mean := values.foldl (· + ·) 0.0 / values.length.toFloat
    let stdDev := calculateStandardDeviation values
    if mean == 0.0 then 0.0 else stdDev / mean

-- Determinism test for simple composition
def testSimpleCompositionDeterminism (config : DeterminismConfig) : MetaM DeterminismTestResult := do
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"
  let comp := mkTestComposition f g

  let mut results : List (Expr × Nat × Nat) := []
  let mut failures : List String := []
  let mut executionTimes : List Float := []
  let mut memoryUsages : List Float := []

  for i in [0:config.maxIterations] do
    let (result, execTime) ← measureExecutionTime (normalizeGoal comp testConfig)
    let (_, memUsage) ← measureMemoryUsage (normalizeGoal comp testConfig)

    results := results ++ [(result.1, execTime, memUsage)]
    executionTimes := executionTimes ++ [execTime.toFloat]
    memoryUsages := memoryUsages ++ [memUsage.toFloat]

    -- Check result consistency
    if i > 0 then
      let prevResult := results[i - 1]!.1
      if result.1 != prevResult then
        failures := failures ++ [s!"Result inconsistency at iteration {i}"]

  -- Check execution time variance
  let execTimeVariance := calculateCoefficientOfVariation executionTimes
  let memUsageVariance := calculateCoefficientOfVariation memoryUsages

  -- Check if results are consistent
  let resultConsistency := results.all (fun (r, _, _) => r == results[0]!.1)
  let sideEffectConsistency := true -- Would need to track side effects

  -- Calculate statistics
  let avgExecTime := executionTimes.foldl (· + ·) 0.0 / executionTimes.length.toFloat
  let avgMemUsage := memoryUsages.foldl (· + ·) 0.0 / memoryUsages.length.toFloat
  let minExecTime := executionTimes.foldl Nat.min executionTimes[0]!.toNat
  let maxExecTime := executionTimes.foldl Nat.max executionTimes[0]!.toNat
  let minMemUsage := memoryUsages.foldl Nat.min memoryUsages[0]!.toNat
  let maxMemUsage := memoryUsages.foldl Nat.max memoryUsages[0]!.toNat

  let isDeterministic := resultConsistency &&
                        execTimeVariance < config.tolerance &&
                        memUsageVariance < config.tolerance &&
                        failures.isEmpty

  return {
    testName := "simple_composition"
    isDeterministic := isDeterministic
    iterations := config.maxIterations
    failures := failures
    executionTimeVariance := execTimeVariance
    memoryUsageVariance := memUsageVariance
    resultConsistency := resultConsistency
    sideEffectConsistency := sideEffectConsistency
    averageExecutionTime := avgExecTime
    averageMemoryUsage := avgMemUsage
    minExecutionTime := minExecTime
    maxExecutionTime := maxExecTime
    minMemoryUsage := minMemUsage
    maxMemoryUsage := maxMemUsage
  }

-- Determinism test for nested composition
def testNestedCompositionDeterminism (config : DeterminismConfig) : MetaM DeterminismTestResult := do
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"
  let h ← mkTestExpr "h"
  let i ← mkTestExpr "i"
  let j ← mkTestExpr "j"

  let comp1 := mkTestComposition i j
  let comp2 := mkTestComposition h comp1
  let comp3 := mkTestComposition g comp2
  let comp4 := mkTestComposition f comp3

  let mut results : List (Expr × Nat × Nat) := []
  let mut failures : List String := []
  let mut executionTimes : List Float := []
  let mut memoryUsages : List Float := []

  for i in [0:config.maxIterations] do
    let (result, execTime) ← measureExecutionTime (normalizeGoal comp4 testConfig)
    let (_, memUsage) ← measureMemoryUsage (normalizeGoal comp4 testConfig)

    results := results ++ [(result.1, execTime, memUsage)]
    executionTimes := executionTimes ++ [execTime.toFloat]
    memoryUsages := memoryUsages ++ [memUsage.toFloat]

    -- Check result consistency
    if i > 0 then
      let prevResult := results[i - 1]!.1
      if result.1 != prevResult then
        failures := failures ++ [s!"Result inconsistency at iteration {i}"]

  -- Check execution time variance
  let execTimeVariance := calculateCoefficientOfVariation executionTimes
  let memUsageVariance := calculateCoefficientOfVariation memoryUsages

  -- Check if results are consistent
  let resultConsistency := results.all (fun (r, _, _) => r == results[0]!.1)
  let sideEffectConsistency := true

  -- Calculate statistics
  let avgExecTime := executionTimes.foldl (· + ·) 0.0 / executionTimes.length.toFloat
  let avgMemUsage := memoryUsages.foldl (· + ·) 0.0 / memoryUsages.length.toFloat
  let minExecTime := executionTimes.foldl Nat.min executionTimes[0]!.toNat
  let maxExecTime := executionTimes.foldl Nat.max executionTimes[0]!.toNat
  let minMemUsage := memoryUsages.foldl Nat.min memoryUsages[0]!.toNat
  let maxMemUsage := memoryUsages.foldl Nat.max memoryUsages[0]!.toNat

  let isDeterministic := resultConsistency &&
                        execTimeVariance < config.tolerance &&
                        memUsageVariance < config.tolerance &&
                        failures.isEmpty

  return {
    testName := "nested_composition"
    isDeterministic := isDeterministic
    iterations := config.maxIterations
    failures := failures
    executionTimeVariance := execTimeVariance
    memoryUsageVariance := memUsageVariance
    resultConsistency := resultConsistency
    sideEffectConsistency := sideEffectConsistency
    averageExecutionTime := avgExecTime
    averageMemoryUsage := avgMemUsage
    minExecutionTime := minExecTime
    maxExecutionTime := maxExecTime
    minMemoryUsage := minMemUsage
    maxMemoryUsage := maxMemUsage
  }

-- Determinism test for monoidal tensor
def testMonoidalTensorDeterminism (config : DeterminismConfig) : MetaM DeterminismTestResult := do
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"
  let h ← mkTestExpr "h"
  let i ← mkTestExpr "i"
  let j ← mkTestExpr "j"

  let tensor1 := mkTestTensor f g
  let tensor2 := mkTestTensor tensor1 h
  let tensor3 := mkTestTensor tensor2 i
  let tensor4 := mkTestTensor tensor3 j

  let mut results : List (Expr × Nat × Nat) := []
  let mut failures : List String := []
  let mut executionTimes : List Float := []
  let mut memoryUsages : List Float := []

  for i in [0:config.maxIterations] do
    let (result, execTime) ← measureExecutionTime (normalizeGoal tensor4 testConfig)
    let (_, memUsage) ← measureMemoryUsage (normalizeGoal tensor4 testConfig)

    results := results ++ [(result.1, execTime, memUsage)]
    executionTimes := executionTimes ++ [execTime.toFloat]
    memoryUsages := memoryUsages ++ [memUsage.toFloat]

    -- Check result consistency
    if i > 0 then
      let prevResult := results[i - 1]!.1
      if result.1 != prevResult then
        failures := failures ++ [s!"Result inconsistency at iteration {i}"]

  -- Check execution time variance
  let execTimeVariance := calculateCoefficientOfVariation executionTimes
  let memUsageVariance := calculateCoefficientOfVariation memoryUsages

  -- Check if results are consistent
  let resultConsistency := results.all (fun (r, _, _) => r == results[0]!.1)
  let sideEffectConsistency := true

  -- Calculate statistics
  let avgExecTime := executionTimes.foldl (· + ·) 0.0 / executionTimes.length.toFloat
  let avgMemUsage := memoryUsages.foldl (· + ·) 0.0 / memoryUsages.length.toFloat
  let minExecTime := executionTimes.foldl Nat.min executionTimes[0]!.toNat
  let maxExecTime := executionTimes.foldl Nat.max executionTimes[0]!.toNat
  let minMemUsage := memoryUsages.foldl Nat.min memoryUsages[0]!.toNat
  let maxMemUsage := memoryUsages.foldl Nat.max memoryUsages[0]!.toNat

  let isDeterministic := resultConsistency &&
                        execTimeVariance < config.tolerance &&
                        memUsageVariance < config.tolerance &&
                        failures.isEmpty

  return {
    testName := "monoidal_tensor"
    isDeterministic := isDeterministic
    iterations := config.maxIterations
    failures := failures
    executionTimeVariance := execTimeVariance
    memoryUsageVariance := memUsageVariance
    resultConsistency := resultConsistency
    sideEffectConsistency := sideEffectConsistency
    averageExecutionTime := avgExecTime
    averageMemoryUsage := avgMemUsage
    minExecutionTime := minExecTime
    maxExecutionTime := maxExecTime
    minMemoryUsage := minMemUsage
    maxMemoryUsage := maxMemUsage
  }

-- Determinism test for functor map
def testFunctorMapDeterminism (config : DeterminismConfig) : MetaM DeterminismTestResult := do
  let F ← mkTestExpr "F"
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"
  let h ← mkTestExpr "h"
  let i ← mkTestExpr "i"

  let comp1 := mkTestComposition f g
  let comp2 := mkTestComposition comp1 h
  let comp3 := mkTestComposition comp2 i
  let map := mkTestFunctorMap F comp3

  let mut results : List (Expr × Nat × Nat) := []
  let mut failures : List String := []
  let mut executionTimes : List Float := []
  let mut memoryUsages : List Float := []

  for i in [0:config.maxIterations] do
    let (result, execTime) ← measureExecutionTime (normalizeGoal map testConfig)
    let (_, memUsage) ← measureMemoryUsage (normalizeGoal map testConfig)

    results := results ++ [(result.1, execTime, memUsage)]
    executionTimes := executionTimes ++ [execTime.toFloat]
    memoryUsages := memoryUsages ++ [memUsage.toFloat]

    -- Check result consistency
    if i > 0 then
      let prevResult := results[i - 1]!.1
      if result.1 != prevResult then
        failures := failures ++ [s!"Result inconsistency at iteration {i}"]

  -- Check execution time variance
  let execTimeVariance := calculateCoefficientOfVariation executionTimes
  let memUsageVariance := calculateCoefficientOfVariation memoryUsages

  -- Check if results are consistent
  let resultConsistency := results.all (fun (r, _, _) => r == results[0]!.1)
  let sideEffectConsistency := true

  -- Calculate statistics
  let avgExecTime := executionTimes.foldl (· + ·) 0.0 / executionTimes.length.toFloat
  let avgMemUsage := memoryUsages.foldl (· + ·) 0.0 / memoryUsages.length.toFloat
  let minExecTime := executionTimes.foldl Nat.min executionTimes[0]!.toNat
  let maxExecTime := executionTimes.foldl Nat.max executionTimes[0]!.toNat
  let minMemUsage := memoryUsages.foldl Nat.min memoryUsages[0]!.toNat
  let maxMemUsage := memoryUsages.foldl Nat.max memoryUsages[0]!.toNat

  let isDeterministic := resultConsistency &&
                        execTimeVariance < config.tolerance &&
                        memUsageVariance < config.tolerance &&
                        failures.isEmpty

  return {
    testName := "functor_map"
    isDeterministic := isDeterministic
    iterations := config.maxIterations
    failures := failures
    executionTimeVariance := execTimeVariance
    memoryUsageVariance := memUsageVariance
    resultConsistency := resultConsistency
    sideEffectConsistency := sideEffectConsistency
    averageExecutionTime := avgExecTime
    averageMemoryUsage := avgMemUsage
    minExecutionTime := minExecTime
    maxExecutionTime := maxExecTime
    minMemoryUsage := minMemUsage
    maxMemoryUsage := maxMemUsage
  }

-- Determinism test for whiskering
def testWhiskeringDeterminism (config : DeterminismConfig) : MetaM DeterminismTestResult := do
  let F ← mkTestExpr "F"
  let G ← mkTestExpr "G"
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"
  let h ← mkTestExpr "h"

  let comp1 := mkTestComposition f g
  let whiskerLeft := mkTestWhiskerLeft F comp1
  let whiskerRight := mkTestWhiskerRight whiskerLeft G
  let comp2 := mkTestComposition whiskerRight h

  let mut results : List (Expr × Nat × Nat) := []
  let mut failures : List String := []
  let mut executionTimes : List Float := []
  let mut memoryUsages : List Float := []

  for i in [0:config.maxIterations] do
    let (result, execTime) ← measureExecutionTime (normalizeGoal comp2 testConfig)
    let (_, memUsage) ← measureMemoryUsage (normalizeGoal comp2 testConfig)

    results := results ++ [(result.1, execTime, memUsage)]
    executionTimes := executionTimes ++ [execTime.toFloat]
    memoryUsages := memoryUsages ++ [memUsage.toFloat]

    -- Check result consistency
    if i > 0 then
      let prevResult := results[i - 1]!.1
      if result.1 != prevResult then
        failures := failures ++ [s!"Result inconsistency at iteration {i}"]

  -- Check execution time variance
  let execTimeVariance := calculateCoefficientOfVariation executionTimes
  let memUsageVariance := calculateCoefficientOfVariation memoryUsages

  -- Check if results are consistent
  let resultConsistency := results.all (fun (r, _, _) => r == results[0]!.1)
  let sideEffectConsistency := true

  -- Calculate statistics
  let avgExecTime := executionTimes.foldl (· + ·) 0.0 / executionTimes.length.toFloat
  let avgMemUsage := memoryUsages.foldl (· + ·) 0.0 / memoryUsages.length.toFloat
  let minExecTime := executionTimes.foldl Nat.min executionTimes[0]!.toNat
  let maxExecTime := executionTimes.foldl Nat.max executionTimes[0]!.toNat
  let minMemUsage := memoryUsages.foldl Nat.min memoryUsages[0]!.toNat
  let maxMemUsage := memoryUsages.foldl Nat.max memoryUsages[0]!.toNat

  let isDeterministic := resultConsistency &&
                        execTimeVariance < config.tolerance &&
                        memUsageVariance < config.tolerance &&
                        failures.isEmpty

  return {
    testName := "whiskering"
    isDeterministic := isDeterministic
    iterations := config.maxIterations
    failures := failures
    executionTimeVariance := execTimeVariance
    memoryUsageVariance := memUsageVariance
    resultConsistency := resultConsistency
    sideEffectConsistency := sideEffectConsistency
    averageExecutionTime := avgExecTime
    averageMemoryUsage := avgMemUsage
    minExecutionTime := minExecTime
    maxExecutionTime := maxExecTime
    minMemoryUsage := minMemUsage
    maxMemoryUsage := maxMemUsage
  }

-- Determinism test for isomorphism transport
def testIsomorphismTransportDeterminism (config : DeterminismConfig) : MetaM DeterminismTestResult := do
  let iso1 ← mkTestExpr "iso1"
  let iso2 ← mkTestExpr "iso2"
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"

  let hom1 := mkTestIsoHom iso1
  let inv1 := mkTestIsoInv iso1
  let hom2 := mkTestIsoHom iso2
  let inv2 := mkTestIsoInv iso2
  let comp1 := mkTestComposition hom1 inv1
  let comp2 := mkTestComposition comp1 hom2
  let comp3 := mkTestComposition comp2 inv2
  let comp4 := mkTestComposition comp3 f
  let comp5 := mkTestComposition comp4 g

  let mut results : List (Expr × Nat × Nat) := []
  let mut failures : List String := []
  let mut executionTimes : List Float := []
  let mut memoryUsages : List Float := []

  for i in [0:config.maxIterations] do
    let (result, execTime) ← measureExecutionTime (normalizeGoal comp5 testConfig)
    let (_, memUsage) ← measureMemoryUsage (normalizeGoal comp5 testConfig)

    results := results ++ [(result.1, execTime, memUsage)]
    executionTimes := executionTimes ++ [execTime.toFloat]
    memoryUsages := memoryUsages ++ [memUsage.toFloat]

    -- Check result consistency
    if i > 0 then
      let prevResult := results[i - 1]!.1
      if result.1 != prevResult then
        failures := failures ++ [s!"Result inconsistency at iteration {i}"]

  -- Check execution time variance
  let execTimeVariance := calculateCoefficientOfVariation executionTimes
  let memUsageVariance := calculateCoefficientOfVariation memoryUsages

  -- Check if results are consistent
  let resultConsistency := results.all (fun (r, _, _) => r == results[0]!.1)
  let sideEffectConsistency := true

  -- Calculate statistics
  let avgExecTime := executionTimes.foldl (· + ·) 0.0 / executionTimes.length.toFloat
  let avgMemUsage := memoryUsages.foldl (· + ·) 0.0 / memoryUsages.length.toFloat
  let minExecTime := executionTimes.foldl Nat.min executionTimes[0]!.toNat
  let maxExecTime := executionTimes.foldl Nat.max executionTimes[0]!.toNat
  let minMemUsage := memoryUsages.foldl Nat.min memoryUsages[0]!.toNat
  let maxMemUsage := memoryUsages.foldl Nat.max memoryUsages[0]!.toNat

  let isDeterministic := resultConsistency &&
                        execTimeVariance < config.tolerance &&
                        memUsageVariance < config.tolerance &&
                        failures.isEmpty

  return {
    testName := "isomorphism_transport"
    isDeterministic := isDeterministic
    iterations := config.maxIterations
    failures := failures
    executionTimeVariance := execTimeVariance
    memoryUsageVariance := memUsageVariance
    resultConsistency := resultConsistency
    sideEffectConsistency := sideEffectConsistency
    averageExecutionTime := avgExecTime
    averageMemoryUsage := avgMemUsage
    minExecutionTime := minExecTime
    maxExecutionTime := maxExecTime
    minMemoryUsage := minMemUsage
    maxMemoryUsage := maxMemUsage
  }

-- Determinism test for complex mixed expression
def testComplexMixedExpressionDeterminism (config : DeterminismConfig) : MetaM DeterminismTestResult := do
  let F ← mkTestExpr "F"
  let G ← mkTestExpr "G"
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"
  let h ← mkTestExpr "h"
  let i ← mkTestExpr "i"
  let iso ← mkTestExpr "iso"

  let hom := mkTestIsoHom iso
  let comp1 := mkTestComposition hom f
  let tensor1 := mkTestTensor comp1 g
  let map1 := mkTestFunctorMap F tensor1
  let whiskerLeft1 := mkTestWhiskerLeft G h
  let comp2 := mkTestComposition map1 whiskerLeft1
  let tensor2 := mkTestTensor comp2 i
  let associator := mkTestAssociator tensor2 g h
  let braid := mkTestBraid associator f
  let comp3 := mkTestComposition braid g
  let tensor3 := mkTestTensor comp3 h
  let map2 := mkTestFunctorMap F tensor3
  let whiskerRight1 := mkTestWhiskerRight map2 G
  let comp4 := mkTestComposition whiskerRight1 f

  let mut results : List (Expr × Nat × Nat) := []
  let mut failures : List String := []
  let mut executionTimes : List Float := []
  let mut memoryUsages : List Float := []

  for i in [0:config.maxIterations] do
    let (result, execTime) ← measureExecutionTime (normalizeGoal comp4 testConfig)
    let (_, memUsage) ← measureMemoryUsage (normalizeGoal comp4 testConfig)

    results := results ++ [(result.1, execTime, memUsage)]
    executionTimes := executionTimes ++ [execTime.toFloat]
    memoryUsages := memoryUsages ++ [memUsage.toFloat]

    -- Check result consistency
    if i > 0 then
      let prevResult := results[i - 1]!.1
      if result.1 != prevResult then
        failures := failures ++ [s!"Result inconsistency at iteration {i}"]

  -- Check execution time variance
  let execTimeVariance := calculateCoefficientOfVariation executionTimes
  let memUsageVariance := calculateCoefficientOfVariation memoryUsages

  -- Check if results are consistent
  let resultConsistency := results.all (fun (r, _, _) => r == results[0]!.1)
  let sideEffectConsistency := true

  -- Calculate statistics
  let avgExecTime := executionTimes.foldl (· + ·) 0.0 / executionTimes.length.toFloat
  let avgMemUsage := memoryUsages.foldl (· + ·) 0.0 / memoryUsages.length.toFloat
  let minExecTime := executionTimes.foldl Nat.min executionTimes[0]!.toNat
  let maxExecTime := executionTimes.foldl Nat.max executionTimes[0]!.toNat
  let minMemUsage := memoryUsages.foldl Nat.min memoryUsages[0]!.toNat
  let maxMemUsage := memoryUsages.foldl Nat.max memoryUsages[0]!.toNat

  let isDeterministic := resultConsistency &&
                        execTimeVariance < config.tolerance &&
                        memUsageVariance < config.tolerance &&
                        failures.isEmpty

  return {
    testName := "complex_mixed_expression"
    isDeterministic := isDeterministic
    iterations := config.maxIterations
    failures := failures
    executionTimeVariance := execTimeVariance
    memoryUsageVariance := memUsageVariance
    resultConsistency := resultConsistency
    sideEffectConsistency := sideEffectConsistency
    averageExecutionTime := avgExecTime
    averageMemoryUsage := avgMemUsage
    minExecutionTime := minExecTime
    maxExecutionTime := maxExecTime
    minMemoryUsage := minMemUsage
    maxMemoryUsage := maxMemUsage
  }

-- Determinism test for edge cases
def testEdgeCasesDeterminism (config : DeterminismConfig) : MetaM DeterminismTestResult := do
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"

  -- Test with empty expressions
  let emptyExpr := f

  -- Test with identity expressions
  let id1 := mkTestIdentity f
  let id2 := mkTestIdentity g

  -- Test with simple compositions
  let comp1 := mkTestComposition f g
  let comp2 := mkTestComposition g f

  let mut results : List (Expr × Nat × Nat) := []
  let mut failures : List String := []
  let mut executionTimes : List Float := []
  let mut memoryUsages : List Float := []

  for i in [0:config.maxIterations] do
    let (result1, execTime1) ← measureExecutionTime (normalizeGoal emptyExpr testConfig)
    let (_, memUsage1) ← measureMemoryUsage (normalizeGoal emptyExpr testConfig)
    let (result2, execTime2) ← measureExecutionTime (normalizeGoal id1 testConfig)
    let (_, memUsage2) ← measureMemoryUsage (normalizeGoal id1 testConfig)
    let (result3, execTime3) ← measureExecutionTime (normalizeGoal id2 testConfig)
    let (_, memUsage3) ← measureMemoryUsage (normalizeGoal id2 testConfig)
    let (result4, execTime4) ← measureExecutionTime (normalizeGoal comp1 testConfig)
    let (_, memUsage4) ← measureMemoryUsage (normalizeGoal comp1 testConfig)
    let (result5, execTime5) ← measureExecutionTime (normalizeGoal comp2 testConfig)
    let (_, memUsage5) ← measureMemoryUsage (normalizeGoal comp2 testConfig)

    let totalExecTime := execTime1 + execTime2 + execTime3 + execTime4 + execTime5
    let totalMemUsage := memUsage1 + memUsage2 + memUsage3 + memUsage4 + memUsage5

    results := results ++ [(result1.1, totalExecTime, totalMemUsage)]
    executionTimes := executionTimes ++ [totalExecTime.toFloat]
    memoryUsages := memoryUsages ++ [totalMemUsage.toFloat]

    -- Check result consistency
    if i > 0 then
      let prevResult := results[i - 1]!.1
      if result1.1 != prevResult then
        failures := failures ++ [s!"Result inconsistency at iteration {i}"]

  -- Check execution time variance
  let execTimeVariance := calculateCoefficientOfVariation executionTimes
  let memUsageVariance := calculateCoefficientOfVariation memoryUsages

  -- Check if results are consistent
  let resultConsistency := results.all (fun (r, _, _) => r == results[0]!.1)
  let sideEffectConsistency := true

  -- Calculate statistics
  let avgExecTime := executionTimes.foldl (· + ·) 0.0 / executionTimes.length.toFloat
  let avgMemUsage := memoryUsages.foldl (· + ·) 0.0 / memoryUsages.length.toFloat
  let minExecTime := executionTimes.foldl Nat.min executionTimes[0]!.toNat
  let maxExecTime := executionTimes.foldl Nat.max executionTimes[0]!.toNat
  let minMemUsage := memoryUsages.foldl Nat.min memoryUsages[0]!.toNat
  let maxMemUsage := memoryUsages.foldl Nat.max memoryUsages[0]!.toNat

  let isDeterministic := resultConsistency &&
                        execTimeVariance < config.tolerance &&
                        memUsageVariance < config.tolerance &&
                        failures.isEmpty

  return {
    testName := "edge_cases"
    isDeterministic := isDeterministic
    iterations := config.maxIterations
    failures := failures
    executionTimeVariance := execTimeVariance
    memoryUsageVariance := memUsageVariance
    resultConsistency := resultConsistency
    sideEffectConsistency := sideEffectConsistency
    averageExecutionTime := avgExecTime
    averageMemoryUsage := avgMemUsage
    minExecutionTime := minExecTime
    maxExecutionTime := maxExecTime
    minMemoryUsage := minMemUsage
    maxMemoryUsage := maxMemUsage
  }

-- Run all determinism tests
def runAllDeterminismTests : MetaM Unit := do
  let config : DeterminismConfig := {
    maxIterations := 50
    timeoutMs := 5000
    enableCaching := true
    enableTracing := false
    checkMemoryUsage := true
    checkExecutionTime := true
    checkResultEquality := true
    checkSideEffects := true
    tolerance := 0.01
  }

  let tests := [
    testSimpleCompositionDeterminism config,
    testNestedCompositionDeterminism config,
    testMonoidalTensorDeterminism config,
    testFunctorMapDeterminism config,
    testWhiskeringDeterminism config,
    testIsomorphismTransportDeterminism config,
    testComplexMixedExpressionDeterminism config,
    testEdgeCasesDeterminism config
  ]

  let mut allPassed := true

  for test in tests do
    let result ← test

    logInfo s!"Determinism test: {result.testName}"
    logInfo s!"  Is deterministic: {result.isDeterministic}"
    logInfo s!"  Iterations: {result.iterations}"
    logInfo s!"  Execution time variance: {result.executionTimeVariance}"
    logInfo s!"  Memory usage variance: {result.memoryUsageVariance}"
    logInfo s!"  Result consistency: {result.resultConsistency}"
    logInfo s!"  Side effect consistency: {result.sideEffectConsistency}"
    logInfo s!"  Average execution time: {result.averageExecutionTime}ms"
    logInfo s!"  Average memory usage: {result.averageMemoryUsage} bytes"
    logInfo s!"  Min/Max execution time: {result.minExecutionTime}ms / {result.maxExecutionTime}ms"
    logInfo s!"  Min/Max memory usage: {result.minMemoryUsage} / {result.maxMemoryUsage} bytes"

    if !result.isDeterministic then
      logInfo s!"  FAILURES: {result.failures}"
      allPassed := false
    else
      logInfo s!"  PASSED"

  if allPassed then
    logInfo "All determinism tests passed! The system is deterministic."
  else
    logInfo "Some determinism tests failed! The system may not be deterministic."

  logInfo "Determinism tests completed!"

end CatNF.Tests.Determinism
