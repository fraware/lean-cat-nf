import Lake
open Lake DSL

package «lean-cat-nf» where
  srcDir := "src"

-- Audit baseline: Mathlib master on 2026-08-24.
require mathlib from git
  "https://github.com/leanprover-community/mathlib4.git" @ "dc84fcbe9e049439c1c36d6db290cc0565f77788"

@[default_target]
lean_lib «CatNF» where
  roots := #[
    `CatNF.Core,
    `CatNF.Cache,
    `CatNF.IndexedRules,
    `CatNF.ParallelProcessing,
    `CatNF.Category.Basic,
    `CatNF.Category.FunctorWhisker,
    `CatNF.Category.IsoTransport,
    `CatNF.Category.Pipeline,
    `CatNF.RewriteRules,
    `CatNF.Monoidal.Core,
    `CatNF.Monoidal.Coherence,
    `CatNF.Attr,
    `CatNF.Tactic]

/-- Pre-register test modules so `lake build test-runner` builds dependencies before the exe root (needed on some setups). -/
lean_lib CatNFTests where
  roots := #[
    `CatNF.Tests.Unit.Core,
    `CatNF.Tests.Unit.Attr,
    `CatNF.Tests.Unit.Monoidal,
    `CatNF.Tests.Unit.RewriteRules,
    `CatNF.Tests.Unit.Tactic,
    `CatNF.Tests.Integration.Workflows,
    `CatNF.Tests.Integration.EndToEnd,
    `CatNF.Tests.Performance.Benchmarks,
    `CatNF.Tests.Performance.RegressionDetection,
    `CatNF.Tests.Performance.ProductionOptimizations,
    `CatNF.Tests.Determinism.DeterminismTests,
    `CatNF.Tests.Determinism.NonDeterminismDetection,
    `CatNF.Tests.TestRunner]

def consoleLinkArgs : Array String :=
  if System.Platform.isWindows then #["-Wl,-subsystem,console"] else #[]

lean_exe «bench» where
  root := `bench.Bench
  supportInterpreter := true
  moreLinkArgs := consoleLinkArgs

@[test_driver]
lean_exe «test-runner» where
  root := `CatNF.Tests.TestRunner
  supportInterpreter := true
  moreLinkArgs := consoleLinkArgs

lean_exe «test-runner-final» where
  root := `test_runner_final
  supportInterpreter := true
  moreLinkArgs := consoleLinkArgs

/-- Run `lake exe doc-gen4 -- CatNF` after adding a compatible `require «doc-gen4»` (see CONTRIBUTING.md). -/
target «docs» : Unit := do
  proc {
    cmd := "lake"
    args := #["exe", "doc-gen4", "--", "CatNF"]
  }
  return .nil
