import Mathlib.CategoryTheory.Category.Basic
import Mathlib.Data.Real.Basic
import Lean.Expr
import Lean.Meta
import Lean.Environment
import Lean.CoreM
import CatNF.Core

namespace CatNF.Bench

open Lean Meta CatNF
open CatNF.MorphismNames

-- Benchmark configuration
structure BenchConfig where
  iterations : Nat := 20
  warmupRuns : Nat := 5
  timeoutMs : Nat := 5000
  traceResults : Bool := false

-- Benchmark result
structure BenchResult where
  name : String
  iterations : Nat
  totalTime : Nat
  avgTime : Float
  minTime : Nat
  maxTime : Nat
  p50 : Nat
  p95 : Nat
  success : Bool
  errorMsg : Option String := none

/-- Loose targets for synthetic expr micro-benchmarks (tune when measuring on CI hardware). -/
def targetP50 : Nat := 10000
def targetP95 : Nat := 30000

def benchNormConfig : Config := {
  maxSteps := 500
  timeoutMs := 1500
  monoidal := true
  trace := false
  simpSet := none
}

def benchmarkTest (name : String) (test : MetaM Unit) (config : BenchConfig) : MetaM BenchResult := do
  let mut times : Array Nat := #[]
  let mut success := true
  let mut errorMsg : Option String := none

  for _ in [0:config.warmupRuns] do
    try
      let startTime ← IO.monoMsNow
      test
      let endTime ← IO.monoMsNow
      let _ := endTime - startTime
    catch _ =>
      success := false
      errorMsg := some "warmup failed"

  for _ in [0:config.iterations] do
    try
      let startTime ← IO.monoMsNow
      test
      let endTime ← IO.monoMsNow
      let duration := endTime - startTime
      times := times.push duration
    catch _ =>
      success := false
      errorMsg := some "iteration failed"
      break

  if not success then
    return {
      name := name
      iterations := 0
      totalTime := 0
      avgTime := 0
      minTime := 0
      maxTime := 0
      p50 := 0
      p95 := 0
      success := false
      errorMsg := errorMsg
    }

  let sortedTimes := times.qsort (· < ·)
  let totalTime := times.foldl (· + ·) 0
  let avgTime := totalTime.toFloat / times.size.toFloat
  let minTime := sortedTimes[0]!
  let maxTime := sortedTimes[sortedTimes.size - 1]!
  let p50Index := times.size / 2
  let p95Index := (times.size * 95) / 100
  let p50 := sortedTimes[p50Index]!
  let p95 := sortedTimes[p95Index]!

  return {
    name := name
    iterations := times.size
    totalTime := totalTime
    avgTime := avgTime
    minTime := minTime
    maxTime := maxTime
    p50 := p50
    p95 := p95
    success := true
    errorMsg := none
  }

def testSyntheticComp : MetaM Unit := do
  let f := mkConst (Name.mkSimple "f")
  let g := mkConst (Name.mkSimple "g")
  let comp := mkCategoryComp f g
  let _ ← normalizeGoalM comp benchNormConfig

def testSyntheticEqComp : MetaM Unit := do
  let f := mkConst (Name.mkSimple "f")
  let g := mkConst (Name.mkSimple "g")
  let h := mkConst (Name.mkSimple "h")
  let left := mkCategoryComp (mkCategoryComp f g) h
  let right := mkCategoryComp f (mkCategoryComp g h)
  let goal := mkApp2 (mkConst `Eq) left right
  let _ ← normalizeGoalM goal benchNormConfig

def testLongChainSynthetic (n : Nat) : MetaM Unit := do
  let f := mkConst (Name.mkSimple "f")
  let g := mkConst (Name.mkSimple "g")
  let mut e := f
  for _ in [0:n] do
    e := mkCategoryComp e g
  let _ ← normalizeGoalM e benchNormConfig

def runBenchmarks (config : BenchConfig) : MetaM (Array BenchResult) := do
  let mut results : Array BenchResult := #[]
  results := results.push (← benchmarkTest "SyntheticComp" testSyntheticComp config)
  results := results.push (← benchmarkTest "SyntheticEqComp" testSyntheticEqComp config)
  results := results.push (← benchmarkTest "LongChain10" (testLongChainSynthetic 10) config)
  results := results.push (← benchmarkTest "LongChain20" (testLongChainSynthetic 20) config)
  return results

def checkPerformanceTargets (results : Array BenchResult) : MetaM Bool := do
  let mut allPassed := true
  for result in results do
    if result.success then
      if result.p50 > targetP50 then
        logError s!"{result.name}: P50 {result.p50}ms > target {targetP50}ms"
        allPassed := false
      if result.p95 > targetP95 then
        logError s!"{result.name}: P95 {result.p95}ms > target {targetP95}ms"
        allPassed := false
    else
      logError s!"{result.name}: Failed with error {result.errorMsg}"
      allPassed := false
  return allPassed

/-- Machine-readable lines for `scripts/compare_benchmarks.py` (prefix `CATNF_BENCH`). -/
def printResults (results : Array BenchResult) : MetaM Unit := do
  IO.println "=== CatNF Benchmark Results ==="
  for result in results do
    if result.success then
      let line :=
        s!"CATNF_BENCH {result.name} p50_ms={result.p50} p95_ms={result.p95} avg_ms={result.avgTime}"
      IO.println line
    else
      IO.println s!"CATNF_BENCH {result.name} FAILED {result.errorMsg.getD ""}"

/-- Entry used from `bench.Bench.main` after imports are loaded; `true` if targets satisfied. -/
def runBenchMain : MetaM Bool := do
  let config : BenchConfig := {
    iterations := 20
    warmupRuns := 5
    timeoutMs := 5000
    traceResults := false
  }
  let results ← runBenchmarks config
  printResults results
  let passed ← checkPerformanceTargets results
  if passed then
    logInfo "All performance targets met!"
  else
    logError "Some performance targets not met!"
  return passed

end CatNF.Bench

open Lean Core Meta

/-- Lake executable root: run benchmarks in a fresh environment (same pattern as Mathlib helpers). -/
unsafe def main : IO UInt32 := do
  Lean.initSearchPath (← Lean.findSysroot)
  withImportModules #[{ module := `bench.Bench }] {} (fun env => do
    let coreCtx : Core.Context := { fileName := "<bench>", fileMap := default }
    let coreS : Core.State := { env }
    let m : CoreM UInt32 := do
      let (passed, _) ← MetaM.run CatNF.Bench.runBenchMain {} {}
      return if passed then 0 else 1
    let (code, _) ← CoreM.toIO m coreCtx coreS
    return code)
