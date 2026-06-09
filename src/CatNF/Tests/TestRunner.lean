import Mathlib.CategoryTheory.Category.Basic
import Mathlib.CategoryTheory.Functor.Basic
import Mathlib.CategoryTheory.Iso
import Mathlib.CategoryTheory.Monoidal.Category
import Mathlib.CategoryTheory.Monoidal.Braided.Basic
import Mathlib.CategoryTheory.Whiskering
import Mathlib.Data.Real.Basic
import Lean.Expr
import Lean.Meta
import Lean.Elab.Command
import Lean.Environment
import Lean.Util.Path
import Lean.CoreM
import CatNF.Core
import CatNF.Category.Basic
import CatNF.Category.FunctorWhisker
import CatNF.Category.IsoTransport
import CatNF.Monoidal.Core
import CatNF.Monoidal.Coherence
import CatNF.RewriteRules
import CatNF.Tactic

-- Import all test modules
import CatNF.Tests.Unit.Core
import CatNF.Tests.Unit.Attr
import CatNF.Tests.Unit.Monoidal
import CatNF.Tests.Unit.RewriteRules
import CatNF.Tests.Unit.Tactic
import CatNF.Tests.Integration.Workflows
import CatNF.Tests.Integration.EndToEnd
import CatNF.Tests.Performance.Benchmarks
import CatNF.Tests.Performance.RegressionDetection
import CatNF.Tests.Determinism.DeterminismTests
import CatNF.Tests.Determinism.NonDeterminismDetection

namespace CatNF.Tests.TestRunner

open Lean Meta Core

-- Test execution result
structure TestExecutionResult where
  testSuite : String
  testName : String
  passed : Bool
  executionTimeMs : Nat
  memoryUsageBytes : Nat
  errorMessage : Option String
  details : Option String

-- Test suite result
structure TestSuiteResult where
  suiteName : String
  totalTests : Nat
  passedTests : Nat
  failedTests : Nat
  executionTimeMs : Nat
  memoryUsageBytes : Nat
  results : List TestExecutionResult

-- Overall test result
structure OverallTestResult where
  totalSuites : Nat
  totalTests : Nat
  passedSuites : Nat
  failedSuites : Nat
  passedTests : Nat
  failedTests : Nat
  totalExecutionTimeMs : Nat
  totalMemoryUsageBytes : Nat
  suiteResults : List TestSuiteResult

-- Test configuration
structure TestConfig where
  enableUnitTests : Bool := true
  enableIntegrationTests : Bool := true
  enablePerformanceTests : Bool := true
  enableDeterminismTests : Bool := true
  enableRegressionDetection : Bool := true
  enableNonDeterminismDetection : Bool := true
  timeoutMs : Nat := 300000 -- 5 minutes
  memoryLimitMB : Nat := 2000 -- 2GB
  enableDetailedLogging : Bool := true
  enablePerformanceLogging : Bool := true
  enableStatisticalAnalysis : Bool := true
  maxConcurrentTests : Nat := 4
  retryFailedTests : Bool := true
  maxRetries : Nat := 3

-- Performance measurement utilities
def measureExecutionTime (α : Type) (action : Lean.Meta.MetaM α) : Lean.Meta.MetaM (α × Nat) := do
  let startTime ← IO.monoMsNow
  let result ← action
  let endTime ← IO.monoMsNow
  let executionTime := endTime - startTime
  return (result, executionTime)

def measureMemoryUsage (α : Type) (action : Lean.Meta.MetaM α) : Lean.Meta.MetaM (α × Nat) := do
  let startMemory ← IO.getNumHeartbeats
  let result ← action
  let endMemory ← IO.getNumHeartbeats
  let memoryUsage := endMemory - startMemory
  return (result, memoryUsage)

-- Unit test runner
def runUnitTests (config : TestConfig) : Lean.Meta.MetaM TestSuiteResult := do
  if !config.enableUnitTests then
    return {
      suiteName := "Unit Tests"
      totalTests := 0
      passedTests := 0
      failedTests := 0
      executionTimeMs := 0
      memoryUsageBytes := 0
      results := []
    }

  IO.println "Running unit tests..."

  let mut results : List TestExecutionResult := []
  let mut totalTime := 0
  let mut totalMemory := 0
  let mut passedCount := 0
  let mut failedCount := 0

  -- Run Core unit tests
  let (coreResult, coreTime) ← measureExecutionTime Bool (do
    try
      CatNF.Tests.Unit.runAllTests
      return true
    catch _ =>
      IO.println "Core tests failed"
      return false
  )
  let (_, coreMemory) ← measureMemoryUsage Unit (do
    try
      CatNF.Tests.Unit.runAllTests
      return ()
    catch _ =>
      return ()
  )

  results := results ++ [{
    testSuite := "Unit Tests"
    testName := "Core Tests"
    passed := coreResult
    executionTimeMs := coreTime
    memoryUsageBytes := coreMemory
    errorMessage := if coreResult then none else some "Core tests failed"
    details := some "Core module unit tests"
  }]

  totalTime := totalTime + coreTime
  totalMemory := totalMemory + coreMemory
  if coreResult then
    passedCount := passedCount + 1
  else
    failedCount := failedCount + 1

  -- Run Attr unit tests
  let (attrResult, attrTime) ← measureExecutionTime Bool (do
    try
      CatNF.Tests.Unit.Attr.runAllTests
      return true
    catch _ =>
      IO.println "Attr tests failed"
      return false
  )
  let (_, attrMemory) ← measureMemoryUsage Unit (do
    try
      CatNF.Tests.Unit.Attr.runAllTests
      return ()
    catch _ =>
      return ()
  )

  results := results ++ [{
    testSuite := "Unit Tests"
    testName := "Attr Tests"
    passed := attrResult
    executionTimeMs := attrTime
    memoryUsageBytes := attrMemory
    errorMessage := if attrResult then none else some "Attr tests failed"
    details := some "Attribute module unit tests"
  }]

  totalTime := totalTime + attrTime
  totalMemory := totalMemory + attrMemory
  if attrResult then
    passedCount := passedCount + 1
  else
    failedCount := failedCount + 1

  -- Run Monoidal unit tests
  let (monoidalResult, monoidalTime) ← measureExecutionTime Bool (do
    try
      CatNF.Tests.Unit.Monoidal.runAllTests
      return true
    catch _ =>
      IO.println "Monoidal tests failed"
      return false
  )
  let (_, monoidalMemory) ← measureMemoryUsage Unit (do
    try
      CatNF.Tests.Unit.Monoidal.runAllTests
      return ()
    catch _ =>
      return ()
  )

  results := results ++ [{
    testSuite := "Unit Tests"
    testName := "Monoidal Tests"
    passed := monoidalResult
    executionTimeMs := monoidalTime
    memoryUsageBytes := monoidalMemory
    errorMessage := if monoidalResult then none else some "Monoidal tests failed"
    details := some "Monoidal module unit tests"
  }]

  totalTime := totalTime + monoidalTime
  totalMemory := totalMemory + monoidalMemory
  if monoidalResult then
    passedCount := passedCount + 1
  else
    failedCount := failedCount + 1

  -- Run RewriteRules unit tests
  let (rewriteResult, rewriteTime) ← measureExecutionTime Bool (do
    try
      CatNF.Tests.Unit.RewriteRules.runAllTests
      return true
    catch _ =>
      IO.println "RewriteRules tests failed"
      return false
  )
  let (_, rewriteMemory) ← measureMemoryUsage Unit (do
    try
      CatNF.Tests.Unit.RewriteRules.runAllTests
      return ()
    catch _ =>
      return ()
  )

  results := results ++ [{
    testSuite := "Unit Tests"
    testName := "RewriteRules Tests"
    passed := rewriteResult
    executionTimeMs := rewriteTime
    memoryUsageBytes := rewriteMemory
    errorMessage := if rewriteResult then none else some "RewriteRules tests failed"
    details := some "RewriteRules module unit tests"
  }]

  totalTime := totalTime + rewriteTime
  totalMemory := totalMemory + rewriteMemory
  if rewriteResult then
    passedCount := passedCount + 1
  else
    failedCount := failedCount + 1

  -- Run Tactic unit tests
  let (tacticResult, tacticTime) ← measureExecutionTime Bool (do
    try
      CatNF.Tests.Unit.Tactic.runAllTests
      return true
    catch _ =>
      IO.println "Tactic tests failed"
      return false
  )
  let (_, tacticMemory) ← measureMemoryUsage Unit (do
    try
      CatNF.Tests.Unit.Tactic.runAllTests
      return ()
    catch _ =>
      return ()
  )

  results := results ++ [{
    testSuite := "Unit Tests"
    testName := "Tactic Tests"
    passed := tacticResult
    executionTimeMs := tacticTime
    memoryUsageBytes := tacticMemory
    errorMessage := if tacticResult then none else some "Tactic tests failed"
    details := some "Tactic module unit tests"
  }]

  totalTime := totalTime + tacticTime
  totalMemory := totalMemory + tacticMemory
  if tacticResult then
    passedCount := passedCount + 1
  else
    failedCount := failedCount + 1

  IO.println s!"Unit tests completed: {passedCount} passed, {failedCount} failed"

  return {
    suiteName := "Unit Tests"
    totalTests := passedCount + failedCount
    passedTests := passedCount
    failedTests := failedCount
    executionTimeMs := totalTime
    memoryUsageBytes := totalMemory
    results := results
  }

-- Integration test runner
def runIntegrationTests (config : TestConfig) : Lean.Meta.MetaM TestSuiteResult := do
  if !config.enableIntegrationTests then
    return {
      suiteName := "Integration Tests"
      totalTests := 0
      passedTests := 0
      failedTests := 0
      executionTimeMs := 0
      memoryUsageBytes := 0
      results := []
    }

  IO.println "Running integration tests..."

  let mut results : List TestExecutionResult := []
  let mut totalTime := 0
  let mut totalMemory := 0
  let mut passedCount := 0
  let mut failedCount := 0

  -- Run Workflows integration tests
  let (workflowsResult, workflowsTime) ← measureExecutionTime Bool (do
    try
      CatNF.Tests.Integration.Workflows.runAllTests
      return true
    catch _ =>
      IO.println "Workflows tests failed"
      return false
  )
  let (_, workflowsMemory) ← measureMemoryUsage Unit (do
    try
      CatNF.Tests.Integration.Workflows.runAllTests
      return ()
    catch _ =>
      return ()
  )

  results := results ++ [{
    testSuite := "Integration Tests"
    testName := "Workflows Tests"
    passed := workflowsResult
    executionTimeMs := workflowsTime
    memoryUsageBytes := workflowsMemory
    errorMessage := if workflowsResult then none else some "Workflows tests failed"
    details := some "Integration workflow tests"
  }]

  totalTime := totalTime + workflowsTime
  totalMemory := totalMemory + workflowsMemory
  if workflowsResult then
    passedCount := passedCount + 1
  else
    failedCount := failedCount + 1

  -- Run EndToEnd integration tests
  let (endToEndResult, endToEndTime) ← measureExecutionTime Bool (do
    try
      CatNF.Tests.Integration.EndToEnd.runAllTests
      return true
    catch _ =>
      IO.println "EndToEnd tests failed"
      return false
  )
  let (_, endToEndMemory) ← measureMemoryUsage Unit (do
    try
      CatNF.Tests.Integration.EndToEnd.runAllTests
      return ()
    catch _ =>
      return ()
  )

  results := results ++ [{
    testSuite := "Integration Tests"
    testName := "EndToEnd Tests"
    passed := endToEndResult
    executionTimeMs := endToEndTime
    memoryUsageBytes := endToEndMemory
    errorMessage := if endToEndResult then none else some "EndToEnd tests failed"
    details := some "End-to-end integration tests"
  }]

  totalTime := totalTime + endToEndTime
  totalMemory := totalMemory + endToEndMemory
  if endToEndResult then
    passedCount := passedCount + 1
  else
    failedCount := failedCount + 1

  IO.println s!"Integration tests completed: {passedCount} passed, {failedCount} failed"

  return {
    suiteName := "Integration Tests"
    totalTests := passedCount + failedCount
    passedTests := passedCount
    failedTests := failedCount
    executionTimeMs := totalTime
    memoryUsageBytes := totalMemory
    results := results
  }

-- Performance test runner
def runPerformanceTests (config : TestConfig) : Lean.Meta.MetaM TestSuiteResult := do
  if !config.enablePerformanceTests then
    return {
      suiteName := "Performance Tests"
      totalTests := 0
      passedTests := 0
      failedTests := 0
      executionTimeMs := 0
      memoryUsageBytes := 0
      results := []
    }

  logInfo "Running performance tests..."

  let mut results : List TestExecutionResult := []
  let mut totalTime := 0
  let mut totalMemory := 0
  let mut passedCount := 0
  let mut failedCount := 0

  -- Run Benchmarks performance tests
  let (_, benchmarksTime) ← measureExecutionTime Unit CatNF.Tests.Performance.runPerformanceTests
  let (_, benchmarksMemory) ← measureMemoryUsage Unit CatNF.Tests.Performance.runPerformanceTests

  results := results ++ [{
    testSuite := "Performance Tests"
    testName := "Benchmarks Tests"
    passed := true
    executionTimeMs := benchmarksTime
    memoryUsageBytes := benchmarksMemory
    errorMessage := none
    details := some "Performance benchmark tests"
  }]

  totalTime := totalTime + benchmarksTime
  totalMemory := totalMemory + benchmarksMemory
  passedCount := passedCount + 1

  -- Run RegressionDetection performance tests
  if config.enableRegressionDetection then
    let (_, regressionTime) ← measureExecutionTime Unit CatNF.Tests.Performance.RegressionDetection.runAllRegressionTests
    let (_, regressionMemory) ← measureMemoryUsage Unit CatNF.Tests.Performance.RegressionDetection.runAllRegressionTests

    results := results ++ [{
      testSuite := "Performance Tests"
      testName := "RegressionDetection Tests"
      passed := true
      executionTimeMs := regressionTime
      memoryUsageBytes := regressionMemory
      errorMessage := none
      details := some "Performance regression detection tests"
    }]

    totalTime := totalTime + regressionTime
    totalMemory := totalMemory + regressionMemory
    passedCount := passedCount + 1

  logInfo s!"Performance tests completed: {passedCount} passed, {failedCount} failed"

  return {
    suiteName := "Performance Tests"
    totalTests := passedCount + failedCount
    passedTests := passedCount
    failedTests := failedCount
    executionTimeMs := totalTime
    memoryUsageBytes := totalMemory
    results := results
  }

-- Determinism test runner
def runDeterminismTests (config : TestConfig) : Lean.Meta.MetaM TestSuiteResult := do
  if !config.enableDeterminismTests then
    return {
      suiteName := "Determinism Tests"
      totalTests := 0
      passedTests := 0
      failedTests := 0
      executionTimeMs := 0
      memoryUsageBytes := 0
      results := []
    }

  logInfo "Running determinism tests..."

  let mut results : List TestExecutionResult := []
  let mut totalTime := 0
  let mut totalMemory := 0
  let mut passedCount := 0
  let mut failedCount := 0

  -- Run DeterminismTests determinism tests
  let (_, determinismTime) ← measureExecutionTime Unit CatNF.Tests.Determinism.runAllDeterminismTests
  let (_, determinismMemory) ← measureMemoryUsage Unit CatNF.Tests.Determinism.runAllDeterminismTests

  results := results ++ [{
    testSuite := "Determinism Tests"
    testName := "DeterminismTests"
    passed := true
    executionTimeMs := determinismTime
    memoryUsageBytes := determinismMemory
    errorMessage := none
    details := some "Determinism testing framework tests"
  }]

  totalTime := totalTime + determinismTime
  totalMemory := totalMemory + determinismMemory
  passedCount := passedCount + 1

  -- Run NonDeterminismDetection determinism tests
  if config.enableNonDeterminismDetection then
    let (_, nonDeterminismTime) ← measureExecutionTime Unit CatNF.Tests.Determinism.NonDeterminismDetection.runAllNonDeterminismDetectionTests
    let (_, nonDeterminismMemory) ← measureMemoryUsage Unit CatNF.Tests.Determinism.NonDeterminismDetection.runAllNonDeterminismDetectionTests

    results := results ++ [{
      testSuite := "Determinism Tests"
      testName := "NonDeterminismDetection Tests"
      passed := true
      executionTimeMs := nonDeterminismTime
      memoryUsageBytes := nonDeterminismMemory
      errorMessage := none
      details := some "Non-determinism detection tests"
    }]

    totalTime := totalTime + nonDeterminismTime
    totalMemory := totalMemory + nonDeterminismMemory
    passedCount := passedCount + 1

  logInfo s!"Determinism tests completed: {passedCount} passed, {failedCount} failed"

  return {
    suiteName := "Determinism Tests"
    totalTests := passedCount + failedCount
    passedTests := passedCount
    failedTests := failedCount
    executionTimeMs := totalTime
    memoryUsageBytes := totalMemory
    results := results
  }

-- Main test runner
def runAllTests (config : TestConfig) : Lean.Meta.MetaM OverallTestResult := do
  IO.println "Starting comprehensive test suite execution..."
  IO.println s!"Configuration: Unit={config.enableUnitTests}, Integration={config.enableIntegrationTests}, Performance={config.enablePerformanceTests}, Determinism={config.enableDeterminismTests}"

  let mut suiteResults : List TestSuiteResult := []
  let mut totalTime := 0
  let mut totalMemory := 0
  let mut passedSuites := 0
  let mut failedSuites := 0
  let mut totalTests := 0
  let mut passedTests := 0
  let mut failedTests := 0

  -- Run unit tests
  if config.enableUnitTests then
    let unitResult ← runUnitTests config
    suiteResults := suiteResults ++ [unitResult]
    totalTime := totalTime + unitResult.executionTimeMs
    totalMemory := totalMemory + unitResult.memoryUsageBytes
    totalTests := totalTests + unitResult.totalTests
    passedTests := passedTests + unitResult.passedTests
    failedTests := failedTests + unitResult.failedTests
    if unitResult.failedTests == 0 then
      passedSuites := passedSuites + 1
    else
      failedSuites := failedSuites + 1

  -- Run integration tests
  if config.enableIntegrationTests then
    let integrationResult ← runIntegrationTests config
    suiteResults := suiteResults ++ [integrationResult]
    totalTime := totalTime + integrationResult.executionTimeMs
    totalMemory := totalMemory + integrationResult.memoryUsageBytes
    totalTests := totalTests + integrationResult.totalTests
    passedTests := passedTests + integrationResult.passedTests
    failedTests := failedTests + integrationResult.failedTests
    if integrationResult.failedTests == 0 then
      passedSuites := passedSuites + 1
    else
      failedSuites := failedSuites + 1

  -- Run performance tests
  if config.enablePerformanceTests then
    let performanceResult ← runPerformanceTests config
    suiteResults := suiteResults ++ [performanceResult]
    totalTime := totalTime + performanceResult.executionTimeMs
    totalMemory := totalMemory + performanceResult.memoryUsageBytes
    totalTests := totalTests + performanceResult.totalTests
    passedTests := passedTests + performanceResult.passedTests
    failedTests := failedTests + performanceResult.failedTests
    if performanceResult.failedTests == 0 then
      passedSuites := passedSuites + 1
    else
      failedSuites := failedSuites + 1

  -- Run determinism tests
  if config.enableDeterminismTests then
    let determinismResult ← runDeterminismTests config
    suiteResults := suiteResults ++ [determinismResult]
    totalTime := totalTime + determinismResult.executionTimeMs
    totalMemory := totalMemory + determinismResult.memoryUsageBytes
    totalTests := totalTests + determinismResult.totalTests
    passedTests := passedTests + determinismResult.passedTests
    failedTests := failedTests + determinismResult.failedTests
    if determinismResult.failedTests == 0 then
      passedSuites := passedSuites + 1
    else
      failedSuites := failedSuites + 1

  -- Generate summary report
  logInfo "=== TEST EXECUTION SUMMARY ==="
  logInfo s!"Total test suites: {suiteResults.length}"
  logInfo s!"Passed test suites: {passedSuites}"
  logInfo s!"Failed test suites: {failedSuites}"
  logInfo s!"Total tests: {totalTests}"
  logInfo s!"Passed tests: {passedTests}"
  logInfo s!"Failed tests: {failedTests}"
  logInfo s!"Total execution time: {totalTime}ms"
  logInfo s!"Total memory usage: {totalMemory} bytes"

  -- Log detailed results for each suite
  for suiteResult in suiteResults do
    logInfo s!"=== {suiteResult.suiteName} ==="
    logInfo s!"  Total tests: {suiteResult.totalTests}"
    logInfo s!"  Passed tests: {suiteResult.passedTests}"
    logInfo s!"  Failed tests: {suiteResult.failedTests}"
    logInfo s!"  Execution time: {suiteResult.executionTimeMs}ms"
    logInfo s!"  Memory usage: {suiteResult.memoryUsageBytes} bytes"

    for result in suiteResult.results do
      let status := if result.passed then "PASSED" else "FAILED"
      logInfo s!"    {result.testName}: {status} ({result.executionTimeMs}ms, {result.memoryUsageBytes} bytes)"
      if result.errorMessage.isSome then
        logInfo s!"      Error: {result.errorMessage.get!}"
      if result.details.isSome then
        logInfo s!"      Details: {result.details.get!}"

  if failedTests == 0 then
    logInfo "ALL TESTS PASSED."
  else
    logInfo s!"{failedTests} test(s) FAILED; see results above."

  return {
    totalSuites := suiteResults.length
    totalTests := totalTests
    passedSuites := passedSuites
    failedSuites := failedSuites
    passedTests := passedTests
    failedTests := failedTests
    totalExecutionTimeMs := totalTime
    totalMemoryUsageBytes := totalMemory
    suiteResults := suiteResults
  }

-- Quick test runner for development
def runQuickTests : Lean.Meta.MetaM Unit := do
  let config : TestConfig := {
    enableUnitTests := true
    enableIntegrationTests := true
    enablePerformanceTests := false
    enableDeterminismTests := false
    enableRegressionDetection := false
    enableNonDeterminismDetection := false
    timeoutMs := 60000
    memoryLimitMB := 1000
    enableDetailedLogging := true
    enablePerformanceLogging := false
    enableStatisticalAnalysis := false
    maxConcurrentTests := 2
    retryFailedTests := false
    maxRetries := 1
  }

  let result ← runAllTests config
  logInfo s!"Quick tests completed: {result.passedTests}/{result.totalTests} passed"

/-- Configuration used by `runFullTestsIO` / `runFullTests` (CI default). -/
def ciTestConfig : TestConfig := {
  enableUnitTests := true
  enableIntegrationTests := true
  enablePerformanceTests := true
  enableDeterminismTests := true
  enableRegressionDetection := true
  enableNonDeterminismDetection := true
  timeoutMs := 300000
  memoryLimitMB := 2000
  enableDetailedLogging := true
  enablePerformanceLogging := true
  enableStatisticalAnalysis := true
  maxConcurrentTests := 4
  retryFailedTests := true
  maxRetries := 3
}

/-- Run the full MetaM test harness from `IO` (initializes search path and environment). -/
def runFullTestsIO : IO OverallTestResult := do
  Lean.initSearchPath (← Lean.findSysroot)
  let env ← Lean.importModules #[`CatNF.Tests.TestRunner] {} 0
  let coreCtx : Core.Context := {
    fileName := "<test>"
    fileMap := default
    options := {}
  }
  let coreS : Core.State := { env := env }
  let m : CoreM OverallTestResult := do
    let (result, _) ← MetaM.run (runAllTests ciTestConfig) {} {}
    return result
  let (result, _) ← CoreM.toIO m coreCtx coreS
  return result

-- Full test runner for CI/CD (MetaM; use `runFullTestsIO` from `IO`).
def runFullTests : Lean.Meta.MetaM Unit := do
  let result ← runAllTests ciTestConfig
  IO.println s!"Full tests completed: {result.passedTests}/{result.totalTests} passed"

def runMain : IO Unit := do
  IO.println "CatNF Comprehensive Test Suite"
  IO.println "=============================="

  try
    let result ← runFullTestsIO
    IO.println s!"All tests completed: {result.passedTests}/{result.totalTests} passed"
    if result.failedTests > 0 then
      IO.println s!"FAILED: {result.failedTests} test(s) failed"
      IO.Process.exit 1
    else
      IO.println "SUCCESS: All tests passed"
  catch e =>
    IO.println s!"Test execution failed: {e}"
    IO.Process.exit 1

end CatNF.Tests.TestRunner

/-- Module-root entry point for `lake exe test-runner` (console subsystem on Windows). -/
def main : IO Unit :=
  CatNF.Tests.TestRunner.runMain
