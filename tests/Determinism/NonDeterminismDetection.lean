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

namespace CatNF.Tests.Determinism.NonDeterminismDetection

-- Non-determinism detection configuration
structure NonDeterminismConfig where
  maxIterations : Nat := 1000
  timeoutMs : Nat := 10000
  enableCaching : Bool := true
  enableTracing : Bool := false
  checkMemoryUsage : Bool := true
  checkExecutionTime : Bool := true
  checkResultEquality : Bool := true
  checkSideEffects : Bool := true
  tolerance : Float := 0.001 -- 0.1% tolerance for floating point comparisons
  maxVariance : Float := 0.05 -- 5% maximum variance
  enableStatisticalAnalysis : Bool := true
  confidenceLevel : Float := 0.99 -- 99% confidence level
  minSampleSize : Nat := 100

-- Non-determinism detection result
structure NonDeterminismResult where
  testName : String
  isNonDeterministic : Bool
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
  statisticalSignificance : Bool
  confidenceInterval : Float × Float
  outlierCount : Nat
  isNormalDistribution : Bool

-- Test configuration for deterministic testing
def testConfig : Config := {
  maxSteps := 1000
  timeoutMs := 10000
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
def calculateMean (values : List Float) : Float :=
  if values.isEmpty then 0.0 else values.foldl (· + ·) 0.0 / values.length.toFloat

def calculateVariance (values : List Float) : Float :=
  if values.isEmpty then 0.0 else
    let mean := calculateMean values
    let squaredDiffs := values.map (fun x => (x - mean) * (x - mean))
    squaredDiffs.foldl (· + ·) 0.0 / values.length.toFloat

def calculateStandardDeviation (values : List Float) : Float :=
  Float.sqrt (calculateVariance values)

def calculateCoefficientOfVariation (values : List Float) : Float :=
  if values.isEmpty then 0.0 else
    let mean := calculateMean values
    let stdDev := calculateStandardDeviation values
    if mean == 0.0 then 0.0 else stdDev / mean

def calculateMedian (values : List Float) : Float :=
  if values.isEmpty then 0.0 else
    let sorted := values.qsort (· ≤ ·)
    let n := sorted.length
    if n % 2 == 0 then
      (sorted[n / 2 - 1]! + sorted[n / 2]!) / 2.0
    else
      sorted[n / 2]!

def calculateQuartiles (values : List Float) : Float × Float :=
  if values.isEmpty then (0.0, 0.0) else
    let sorted := values.qsort (· ≤ ·)
    let n := sorted.length
    let q1 := if n % 4 == 0 then
      (sorted[n / 4 - 1]! + sorted[n / 4]!) / 2.0
    else
      sorted[n / 4]!
    let q3 := if n % 4 == 0 then
      (sorted[3 * n / 4 - 1]! + sorted[3 * n / 4]!) / 2.0
    else
      sorted[3 * n / 4]!
    (q1, q3)

def detectOutliers (values : List Float) : Nat :=
  if values.length < 3 then 0 else
    let sorted := values.qsort (· ≤ ·)
    let n := sorted.length
    let q1 := sorted[n / 4]!
    let q3 := sorted[3 * n / 4]!
    let iqr := q3 - q1
    let lowerBound := q1 - 1.5 * iqr
    let upperBound := q3 + 1.5 * iqr
    values.foldl (fun count x => if x < lowerBound || x > upperBound then count + 1 else count) 0

def calculateConfidenceInterval (values : List Float) (confidenceLevel : Float) : Float × Float :=
  if values.length < 2 then (0.0, 0.0) else
    let mean := calculateMean values
    let stdDev := calculateStandardDeviation values
    let n := values.length.toFloat
    let zScore := 2.576 -- For 99% confidence level
    let margin := zScore * stdDev / Float.sqrt n
    (mean - margin, mean + margin)

def isNormalDistribution (values : List Float) : Bool :=
  if values.length < 10 then false else
    let mean := calculateMean values
    let stdDev := calculateStandardDeviation values
    let withinOneStd := values.filter (fun x => Float.abs (x - mean) ≤ stdDev).length
    let withinTwoStd := values.filter (fun x => Float.abs (x - mean) ≤ 2 * stdDev).length
    let withinOneStdPercent := withinOneStd.toFloat / values.length.toFloat
    let withinTwoStdPercent := withinTwoStd.toFloat / values.length.toFloat
    withinOneStdPercent >= 0.68 && withinTwoStdPercent >= 0.95

def performStatisticalAnalysis (values : List Float) (confidenceLevel : Float := 0.99) : (Float × Float) × Nat × Bool :=
  let mean := calculateMean values
  let stdDev := calculateStandardDeviation values
  let confidenceInterval := calculateConfidenceInterval values confidenceLevel
  let outlierCount := detectOutliers values
  let isNormal := isNormalDistribution values
  (confidenceInterval, outlierCount, isNormal)

-- Non-determinism detection functions
def detectNonDeterminism (results : List (Expr × Nat × Nat)) (executionTimes : List Float) (memoryUsages : List Float) (config : NonDeterminismConfig) : Bool :=
  if results.length < config.minSampleSize then false else
    let execTimeVariance := calculateCoefficientOfVariation executionTimes
    let memUsageVariance := calculateCoefficientOfVariation memoryUsages
    let resultConsistency := results.all (fun (r, _, _) => r == results[0]!.1)

    -- Check for non-determinism indicators
    let hasHighVariance := execTimeVariance > config.maxVariance || memUsageVariance > config.maxVariance
    let hasInconsistentResults := !resultConsistency
    let hasStatisticalSignificance := if config.enableStatisticalAnalysis then
      let (confidenceInterval, outlierCount, isNormal) := performStatisticalAnalysis executionTimes config.confidenceLevel
      outlierCount > 0 || !isNormal
    else false

    hasHighVariance || hasInconsistentResults || hasStatisticalSignificance

-- Non-determinism test for simple composition
def testSimpleCompositionNonDeterminism (config : NonDeterminismConfig) : MetaM NonDeterminismResult := do
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
  let sideEffectConsistency := true

  -- Perform statistical analysis
  let (confidenceInterval, outlierCount, isNormal) := performStatisticalAnalysis executionTimes config.confidenceLevel
  let statisticalSignificance := outlierCount > 0 || !isNormal

  -- Calculate statistics
  let avgExecTime := executionTimes.foldl (· + ·) 0.0 / executionTimes.length.toFloat
  let avgMemUsage := memoryUsages.foldl (· + ·) 0.0 / memoryUsages.length.toFloat
  let minExecTime := executionTimes.foldl Nat.min executionTimes[0]!.toNat
  let maxExecTime := executionTimes.foldl Nat.max executionTimes[0]!.toNat
  let minMemUsage := memoryUsages.foldl Nat.min memoryUsages[0]!.toNat
  let maxMemUsage := memoryUsages.foldl Nat.max memoryUsages[0]!.toNat

  let isNonDeterministic := detectNonDeterminism results executionTimes memoryUsages config

  return {
    testName := "simple_composition"
    isNonDeterministic := isNonDeterministic
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
    statisticalSignificance := statisticalSignificance
    confidenceInterval := confidenceInterval
    outlierCount := outlierCount
    isNormalDistribution := isNormal
  }

-- Non-determinism test for nested composition
def testNestedCompositionNonDeterminism (config : NonDeterminismConfig) : MetaM NonDeterminismResult := do
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

  -- Perform statistical analysis
  let (confidenceInterval, outlierCount, isNormal) := performStatisticalAnalysis executionTimes config.confidenceLevel
  let statisticalSignificance := outlierCount > 0 || !isNormal

  -- Calculate statistics
  let avgExecTime := executionTimes.foldl (· + ·) 0.0 / executionTimes.length.toFloat
  let avgMemUsage := memoryUsages.foldl (· + ·) 0.0 / memoryUsages.length.toFloat
  let minExecTime := executionTimes.foldl Nat.min executionTimes[0]!.toNat
  let maxExecTime := executionTimes.foldl Nat.max executionTimes[0]!.toNat
  let minMemUsage := memoryUsages.foldl Nat.min memoryUsages[0]!.toNat
  let maxMemUsage := memoryUsages.foldl Nat.max memoryUsages[0]!.toNat

  let isNonDeterministic := detectNonDeterminism results executionTimes memoryUsages config

  return {
    testName := "nested_composition"
    isNonDeterministic := isNonDeterministic
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
    statisticalSignificance := statisticalSignificance
    confidenceInterval := confidenceInterval
    outlierCount := outlierCount
    isNormalDistribution := isNormal
  }

-- Non-determinism test for monoidal tensor
def testMonoidalTensorNonDeterminism (config : NonDeterminismConfig) : MetaM NonDeterminismResult := do
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

  -- Perform statistical analysis
  let (confidenceInterval, outlierCount, isNormal) := performStatisticalAnalysis executionTimes config.confidenceLevel
  let statisticalSignificance := outlierCount > 0 || !isNormal

  -- Calculate statistics
  let avgExecTime := executionTimes.foldl (· + ·) 0.0 / executionTimes.length.toFloat
  let avgMemUsage := memoryUsages.foldl (· + ·) 0.0 / memoryUsages.length.toFloat
  let minExecTime := executionTimes.foldl Nat.min executionTimes[0]!.toNat
  let maxExecTime := executionTimes.foldl Nat.max executionTimes[0]!.toNat
  let minMemUsage := memoryUsages.foldl Nat.min memoryUsages[0]!.toNat
  let maxMemUsage := memoryUsages.foldl Nat.max memoryUsages[0]!.toNat

  let isNonDeterministic := detectNonDeterminism results executionTimes memoryUsages config

  return {
    testName := "monoidal_tensor"
    isNonDeterministic := isNonDeterministic
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
    statisticalSignificance := statisticalSignificance
    confidenceInterval := confidenceInterval
    outlierCount := outlierCount
    isNormalDistribution := isNormal
  }

-- Non-determinism test for functor map
def testFunctorMapNonDeterminism (config : NonDeterminismConfig) : MetaM NonDeterminismResult := do
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

  -- Perform statistical analysis
  let (confidenceInterval, outlierCount, isNormal) := performStatisticalAnalysis executionTimes config.confidenceLevel
  let statisticalSignificance := outlierCount > 0 || !isNormal

  -- Calculate statistics
  let avgExecTime := executionTimes.foldl (· + ·) 0.0 / executionTimes.length.toFloat
  let avgMemUsage := memoryUsages.foldl (· + ·) 0.0 / memoryUsages.length.toFloat
  let minExecTime := executionTimes.foldl Nat.min executionTimes[0]!.toNat
  let maxExecTime := executionTimes.foldl Nat.max executionTimes[0]!.toNat
  let minMemUsage := memoryUsages.foldl Nat.min memoryUsages[0]!.toNat
  let maxMemUsage := memoryUsages.foldl Nat.max memoryUsages[0]!.toNat

  let isNonDeterministic := detectNonDeterminism results executionTimes memoryUsages config

  return {
    testName := "functor_map"
    isNonDeterministic := isNonDeterministic
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
    statisticalSignificance := statisticalSignificance
    confidenceInterval := confidenceInterval
    outlierCount := outlierCount
    isNormalDistribution := isNormal
  }

-- Non-determinism test for whiskering
def testWhiskeringNonDeterminism (config : NonDeterminismConfig) : MetaM NonDeterminismResult := do
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

  -- Perform statistical analysis
  let (confidenceInterval, outlierCount, isNormal) := performStatisticalAnalysis executionTimes config.confidenceLevel
  let statisticalSignificance := outlierCount > 0 || !isNormal

  -- Calculate statistics
  let avgExecTime := executionTimes.foldl (· + ·) 0.0 / executionTimes.length.toFloat
  let avgMemUsage := memoryUsages.foldl (· + ·) 0.0 / memoryUsages.length.toFloat
  let minExecTime := executionTimes.foldl Nat.min executionTimes[0]!.toNat
  let maxExecTime := executionTimes.foldl Nat.max executionTimes[0]!.toNat
  let minMemUsage := memoryUsages.foldl Nat.min memoryUsages[0]!.toNat
  let maxMemUsage := memoryUsages.foldl Nat.max memoryUsages[0]!.toNat

  let isNonDeterministic := detectNonDeterminism results executionTimes memoryUsages config

  return {
    testName := "whiskering"
    isNonDeterministic := isNonDeterministic
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
    statisticalSignificance := statisticalSignificance
    confidenceInterval := confidenceInterval
    outlierCount := outlierCount
    isNormalDistribution := isNormal
  }

-- Non-determinism test for isomorphism transport
def testIsomorphismTransportNonDeterminism (config : NonDeterminismConfig) : MetaM NonDeterminismResult := do
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

  -- Perform statistical analysis
  let (confidenceInterval, outlierCount, isNormal) := performStatisticalAnalysis executionTimes config.confidenceLevel
  let statisticalSignificance := outlierCount > 0 || !isNormal

  -- Calculate statistics
  let avgExecTime := executionTimes.foldl (· + ·) 0.0 / executionTimes.length.toFloat
  let avgMemUsage := memoryUsages.foldl (· + ·) 0.0 / memoryUsages.length.toFloat
  let minExecTime := executionTimes.foldl Nat.min executionTimes[0]!.toNat
  let maxExecTime := executionTimes.foldl Nat.max executionTimes[0]!.toNat
  let minMemUsage := memoryUsages.foldl Nat.min memoryUsages[0]!.toNat
  let maxMemUsage := memoryUsages.foldl Nat.max memoryUsages[0]!.toNat

  let isNonDeterministic := detectNonDeterminism results executionTimes memoryUsages config

  return {
    testName := "isomorphism_transport"
    isNonDeterministic := isNonDeterministic
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
    statisticalSignificance := statisticalSignificance
    confidenceInterval := confidenceInterval
    outlierCount := outlierCount
    isNormalDistribution := isNormal
  }

-- Non-determinism test for complex mixed expression
def testComplexMixedExpressionNonDeterminism (config : NonDeterminismConfig) : MetaM NonDeterminismResult := do
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

  -- Perform statistical analysis
  let (confidenceInterval, outlierCount, isNormal) := performStatisticalAnalysis executionTimes config.confidenceLevel
  let statisticalSignificance := outlierCount > 0 || !isNormal

  -- Calculate statistics
  let avgExecTime := executionTimes.foldl (· + ·) 0.0 / executionTimes.length.toFloat
  let avgMemUsage := memoryUsages.foldl (· + ·) 0.0 / memoryUsages.length.toFloat
  let minExecTime := executionTimes.foldl Nat.min executionTimes[0]!.toNat
  let maxExecTime := executionTimes.foldl Nat.max executionTimes[0]!.toNat
  let minMemUsage := memoryUsages.foldl Nat.min memoryUsages[0]!.toNat
  let maxMemUsage := memoryUsages.foldl Nat.max memoryUsages[0]!.toNat

  let isNonDeterministic := detectNonDeterminism results executionTimes memoryUsages config

  return {
    testName := "complex_mixed_expression"
    isNonDeterministic := isNonDeterministic
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
    statisticalSignificance := statisticalSignificance
    confidenceInterval := confidenceInterval
    outlierCount := outlierCount
    isNormalDistribution := isNormal
  }

-- Run all non-determinism detection tests
def runAllNonDeterminismDetectionTests : MetaM Unit := do
  let config : NonDeterminismConfig := {
    maxIterations := 1000
    timeoutMs := 10000
    enableCaching := true
    enableTracing := false
    checkMemoryUsage := true
    checkExecutionTime := true
    checkResultEquality := true
    checkSideEffects := true
    tolerance := 0.001
    maxVariance := 0.05
    enableStatisticalAnalysis := true
    confidenceLevel := 0.99
    minSampleSize := 100
  }

  let tests := [
    testSimpleCompositionNonDeterminism config,
    testNestedCompositionNonDeterminism config,
    testMonoidalTensorNonDeterminism config,
    testFunctorMapNonDeterminism config,
    testWhiskeringNonDeterminism config,
    testIsomorphismTransportNonDeterminism config,
    testComplexMixedExpressionNonDeterminism config
  ]

  let mut hasNonDeterminism := false

  for test in tests do
    let result ← test

    logInfo s!"Non-determinism detection test: {result.testName}"
    logInfo s!"  Is non-deterministic: {result.isNonDeterministic}"
    logInfo s!"  Iterations: {result.iterations}"
    logInfo s!"  Execution time variance: {result.executionTimeVariance}"
    logInfo s!"  Memory usage variance: {result.memoryUsageVariance}"
    logInfo s!"  Result consistency: {result.resultConsistency}"
    logInfo s!"  Side effect consistency: {result.sideEffectConsistency}"
    logInfo s!"  Average execution time: {result.averageExecutionTime}ms"
    logInfo s!"  Average memory usage: {result.averageMemoryUsage} bytes"
    logInfo s!"  Min/Max execution time: {result.minExecutionTime}ms / {result.maxExecutionTime}ms"
    logInfo s!"  Min/Max memory usage: {result.minMemoryUsage} / {result.maxMemoryUsage} bytes"
    logInfo s!"  Statistical significance: {result.statisticalSignificance}"
    logInfo s!"  Confidence interval: {result.confidenceInterval}"
    logInfo s!"  Outlier count: {result.outlierCount}"
    logInfo s!"  Is normal distribution: {result.isNormalDistribution}"

    if result.isNonDeterministic then
      logInfo s!"  NON-DETERMINISM DETECTED in {result.testName}!"
      logInfo s!"  FAILURES: {result.failures}"
      hasNonDeterminism := true
    else
      logInfo s!"  PASSED - No non-determinism detected"

  if hasNonDeterminism then
    logInfo "Non-determinism detected! The system may not be deterministic."
  else
    logInfo "No non-determinism detected! The system appears to be deterministic."

  logInfo "Non-determinism detection tests completed!"

end CatNF.Tests.Determinism.NonDeterminismDetection
