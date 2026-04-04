import Lake
open Lake DSL

package «lean-cat-nf» where
  srcDir := "src"

require mathlib from git
  "https://github.com/leanprover-community/mathlib4.git" @ "v4.8.0"

@[default_target]
lean_lib «CatNF» where
  roots := #[
    `CatNF.Core,
    `CatNF.Cache,
    `CatNF.IndexedRules,
    `CatNF.ParallelProcessing,
    `CatNF.AssocUnit,
    `CatNF.FunctorWhisker,
    `CatNF.IsoTransport,
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

lean_exe «bench» where
  root := `bench.Bench
  supportInterpreter := true

@[test_runner]
lean_exe «test-runner» where
  root := `CatNF.Tests.TestRunner
  supportInterpreter := true

lean_exe «test-runner-final» where
  root := `test_runner_final
  supportInterpreter := true

/-- Run `lake exe doc-gen4 -- CatNF` after adding a compatible `require «doc-gen4»` (see CONTRIBUTING.md). -/
target «docs» : Unit := do
  proc {
    cmd := "lake"
    args := #["exe", "doc-gen4", "--", "CatNF"]
  }
  return .nil
