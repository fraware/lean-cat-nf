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

namespace CatNF.Tests.Performance.RegressionDetection

-- Performance measurement utilities
structure PerformanceMetrics where
  executionTimeMs : Nat
  memoryUsageBytes : Nat
  stepsExecuted : Nat
  expressionsProcessed : Nat
  averageTimePerExpression : Float
  peakMemoryUsage : Nat
  cacheHits : Nat
  cacheMisses : Nat
  timestamp : Nat

-- Regression detection configuration
structure RegressionConfig where
  maxExecutionTimeIncrease : Float := 1.5 -- 50% increase
  maxMemoryUsageIncrease : Float := 1.5 -- 50% increase
  maxAverageTimeIncrease : Float := 1.5 -- 50% increase
  maxPeakMemoryIncrease : Float := 1.5 -- 50% increase
  minCacheHitRate : Float := 0.8 -- 80% cache hit rate
  maxExecutionTimeMs : Nat := 10000 -- 10 seconds
  maxMemoryUsageMB : Nat := 1000 -- 1GB
  enableStatisticalAnalysis : Bool := true
  confidenceLevel : Float := 0.95 -- 95% confidence
  minSampleSize : Nat := 10

-- Statistical analysis utilities
structure StatisticalAnalysis where
  mean : Float
  standardDeviation : Float
  variance : Float
  min : Float
  max : Float
  median : Float
  quartile25 : Float
  quartile75 : Float
  confidenceInterval : Float × Float
  outlierCount : Nat
  isNormalDistribution : Bool

-- Performance baseline storage
structure PerformanceBaseline where
  name : String
  metrics : PerformanceMetrics
  statisticalAnalysis : StatisticalAnalysis
  sampleSize : Nat
  lastUpdated : Nat

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

def measurePerformance (action : MetaM α) : MetaM (α × PerformanceMetrics) := do
  let (result, executionTime) ← measureExecutionTime action
  let (_, memoryUsage) ← measureMemoryUsage action
  let timestamp ← IO.monoMsNow

  let metrics : PerformanceMetrics := {
    executionTimeMs := executionTime
    memoryUsageBytes := memoryUsage * 1024
    stepsExecuted := 0
    expressionsProcessed := 1
    averageTimePerExpression := executionTime.toFloat
    peakMemoryUsage := memoryUsage * 1024
    cacheHits := 0
    cacheMisses := 0
    timestamp := timestamp
  }

  return (result, metrics)

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
    let zScore := 1.96 -- For 95% confidence level
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

def performStatisticalAnalysis (values : List Float) (confidenceLevel : Float := 0.95) : StatisticalAnalysis :=
  let mean := calculateMean values
  let variance := calculateVariance values
  let stdDev := calculateStandardDeviation values
  let min := values.foldl Float.min Float.infinity
  let max := values.foldl Float.max (-Float.infinity)
  let median := calculateMedian values
  let (q25, q75) := calculateQuartiles values
  let confidenceInterval := calculateConfidenceInterval values confidenceLevel
  let outlierCount := detectOutliers values
  let isNormal := isNormalDistribution values

  {
    mean := mean
    standardDeviation := stdDev
    variance := variance
    min := min
    max := max
    median := median
    quartile25 := q25
    quartile75 := q75
    confidenceInterval := confidenceInterval
    outlierCount := outlierCount
    isNormalDistribution := isNormal
  }

-- Regression detection functions
def detectPerformanceRegression (baseline : PerformanceBaseline) (current : PerformanceMetrics) (config : RegressionConfig) : Bool :=
  let timeIncrease := current.executionTimeMs.toFloat / baseline.metrics.executionTimeMs.toFloat
  let memoryIncrease := current.memoryUsageBytes.toFloat / baseline.metrics.memoryUsageBytes.toFloat
  let avgTimeIncrease := current.averageTimePerExpression / baseline.metrics.averageTimePerExpression
  let peakMemoryIncrease := current.peakMemoryUsage.toFloat / baseline.metrics.peakMemoryUsage.toFloat
  let cacheHitRate := current.cacheHits.toFloat / (current.cacheHits + current.cacheMisses).toFloat

  timeIncrease > config.maxExecutionTimeIncrease ||
  memoryIncrease > config.maxMemoryUsageIncrease ||
  avgTimeIncrease > config.maxAverageTimeIncrease ||
  peakMemoryIncrease > config.maxPeakMemoryIncrease ||
  cacheHitRate < config.minCacheHitRate ||
  current.executionTimeMs > config.maxExecutionTimeMs ||
  current.memoryUsageBytes > config.maxMemoryUsageMB * 1024 * 1024

def detectStatisticalRegression (baseline : PerformanceBaseline) (currentSamples : List PerformanceMetrics) (config : RegressionConfig) : Bool :=
  if !config.enableStatisticalAnalysis || currentSamples.length < config.minSampleSize then
    false
  else
    let currentTimes := currentSamples.map (fun m => m.executionTimeMs.toFloat)
    let currentAnalysis := performStatisticalAnalysis currentTimes config.confidenceLevel
    let baselineAnalysis := baseline.statisticalAnalysis

    -- Check if current performance is significantly worse than baseline
    let timeIncrease := currentAnalysis.mean / baselineAnalysis.mean
    let memoryIncrease := currentSamples.map (fun m => m.memoryUsageBytes.toFloat) |> calculateMean |> (· / baseline.metrics.memoryUsageBytes.toFloat)

    timeIncrease > config.maxExecutionTimeIncrease ||
    memoryIncrease > config.maxMemoryUsageIncrease ||
    currentAnalysis.mean > baselineAnalysis.mean + 2 * baselineAnalysis.standardDeviation

-- Performance baseline management
def createPerformanceBaseline (name : String) (samples : List PerformanceMetrics) : PerformanceBaseline :=
  let times := samples.map (fun m => m.executionTimeMs.toFloat)
  let analysis := performStatisticalAnalysis times
  let representative := samples.head!

  {
    name := name
    metrics := representative
    statisticalAnalysis := analysis
    sampleSize := samples.length
    lastUpdated := representative.timestamp
  }

def updatePerformanceBaseline (baseline : PerformanceBaseline) (newSamples : List PerformanceMetrics) : PerformanceBaseline :=
  let allSamples := baseline.metrics :: newSamples
  let times := allSamples.map (fun m => m.executionTimeMs.toFloat)
  let analysis := performStatisticalAnalysis times
  let representative := allSamples.head!

  {
    name := baseline.name
    metrics := representative
    statisticalAnalysis := analysis
    sampleSize := allSamples.length
    lastUpdated := representative.timestamp
  }

-- Benchmark functions for regression testing
def benchmarkSimpleComposition (iterations : Nat) : MetaM (List PerformanceMetrics) := do
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"
  let comp := mkTestComposition f g

  let mut results : List PerformanceMetrics := []

  for _ in [0:iterations] do
    let (_, metrics) ← measurePerformance (normalizeGoal comp testConfig)
    results := results ++ [metrics]

  return results

def benchmarkNestedComposition (iterations : Nat) : MetaM (List PerformanceMetrics) := do
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"
  let h ← mkTestExpr "h"
  let i ← mkTestExpr "i"
  let j ← mkTestExpr "j"

  let comp1 := mkTestComposition i j
  let comp2 := mkTestComposition h comp1
  let comp3 := mkTestComposition g comp2
  let comp4 := mkTestComposition f comp3

  let mut results : List PerformanceMetrics := []

  for _ in [0:iterations] do
    let (_, metrics) ← measurePerformance (normalizeGoal comp4 testConfig)
    results := results ++ [metrics]

  return results

def benchmarkMonoidalTensor (iterations : Nat) : MetaM (List PerformanceMetrics) := do
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"
  let h ← mkTestExpr "h"
  let i ← mkTestExpr "i"
  let j ← mkTestExpr "j"

  let tensor1 := mkTestTensor f g
  let tensor2 := mkTestTensor tensor1 h
  let tensor3 := mkTestTensor tensor2 i
  let tensor4 := mkTestTensor tensor3 j

  let mut results : List PerformanceMetrics := []

  for _ in [0:iterations] do
    let (_, metrics) ← measurePerformance (normalizeGoal tensor4 testConfig)
    results := results ++ [metrics]

  return results

def benchmarkFunctorMap (iterations : Nat) : MetaM (List PerformanceMetrics) := do
  let F ← mkTestExpr "F"
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"
  let h ← mkTestExpr "h"
  let i ← mkTestExpr "i"

  let comp1 := mkTestComposition f g
  let comp2 := mkTestComposition comp1 h
  let comp3 := mkTestComposition comp2 i
  let map := mkTestFunctorMap F comp3

  let mut results : List PerformanceMetrics := []

  for _ in [0:iterations] do
    let (_, metrics) ← measurePerformance (normalizeGoal map testConfig)
    results := results ++ [metrics]

  return results

def benchmarkWhiskering (iterations : Nat) : MetaM (List PerformanceMetrics) := do
  let F ← mkTestExpr "F"
  let G ← mkTestExpr "G"
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"
  let h ← mkTestExpr "h"

  let comp1 := mkTestComposition f g
  let whiskerLeft := mkTestWhiskerLeft F comp1
  let whiskerRight := mkTestWhiskerRight whiskerLeft G
  let comp2 := mkTestComposition whiskerRight h

  let mut results : List PerformanceMetrics := []

  for _ in [0:iterations] do
    let (_, metrics) ← measurePerformance (normalizeGoal comp2 testConfig)
    results := results ++ [metrics]

  return results

def benchmarkIsomorphismTransport (iterations : Nat) : MetaM (List PerformanceMetrics) := do
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

  let mut results : List PerformanceMetrics := []

  for _ in [0:iterations] do
    let (_, metrics) ← measurePerformance (normalizeGoal comp5 testConfig)
    results := results ++ [metrics]

  return results

def benchmarkComplexMixedExpression (iterations : Nat) : MetaM (List PerformanceMetrics) := do
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

  let mut results : List PerformanceMetrics := []

  for _ in [0:iterations] do
    let (_, metrics) ← measurePerformance (normalizeGoal comp4 testConfig)
    results := results ++ [metrics]

  return results

-- Regression test runner
def runRegressionTests (config : RegressionConfig) : MetaM Unit := do
  let iterations := 20
  let benchmarks := [
    ("simple_composition", benchmarkSimpleComposition iterations),
    ("nested_composition", benchmarkNestedComposition iterations),
    ("monoidal_tensor", benchmarkMonoidalTensor iterations),
    ("functor_map", benchmarkFunctorMap iterations),
    ("whiskering", benchmarkWhiskering iterations),
    ("isomorphism_transport", benchmarkIsomorphismTransport iterations),
    ("complex_mixed_expression", benchmarkComplexMixedExpression iterations)
  ]

  let mut hasRegression := false

  for (name, benchmark) in benchmarks do
    logInfo s!"Running regression test for {name}..."

    let samples ← benchmark
    let currentMetrics := samples.head!

    -- Create a mock baseline (in real implementation, this would be loaded from storage)
    let mockBaseline := createPerformanceBaseline name samples

    -- Check for regression
    let isRegression := detectPerformanceRegression mockBaseline currentMetrics config
    let isStatisticalRegression := detectStatisticalRegression mockBaseline samples config

    if isRegression || isStatisticalRegression then
      logInfo s!"REGRESSION DETECTED in {name}!"
      hasRegression := true
    else
      logInfo s!"No regression detected in {name}"

    -- Log detailed metrics
    logInfo s!"  Execution time: {currentMetrics.executionTimeMs}ms"
    logInfo s!"  Memory usage: {currentMetrics.memoryUsageBytes} bytes"
    logInfo s!"  Average time per expression: {currentMetrics.averageTimePerExpression}ms"
    logInfo s!"  Peak memory usage: {currentMetrics.peakMemoryUsage} bytes"
    logInfo s!"  Cache hit rate: {currentMetrics.cacheHits.toFloat / (currentMetrics.cacheHits + currentMetrics.cacheMisses).toFloat}"

  if hasRegression then
    logInfo "Performance regression detected! Check the results above."
  else
    logInfo "All regression tests passed! No performance regressions detected."

  logInfo "Regression tests completed!"

-- Main regression test runner
def runAllRegressionTests : MetaM Unit := do
  let config : RegressionConfig := {
    maxExecutionTimeIncrease := 1.5
    maxMemoryUsageIncrease := 1.5
    maxAverageTimeIncrease := 1.5
    maxPeakMemoryIncrease := 1.5
    minCacheHitRate := 0.8
    maxExecutionTimeMs := 10000
    maxMemoryUsageMB := 1000
    enableStatisticalAnalysis := true
    confidenceLevel := 0.95
    minSampleSize := 10
  }

  runRegressionTests config

end CatNF.Tests.Performance.RegressionDetection
