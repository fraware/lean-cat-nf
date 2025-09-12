import Lake
open Lake DSL

package «lean-cat-nf» where
  -- Add package configuration options here

require mathlib from git
  "https://github.com/leanprover-community/mathlib4.git" @ "v4.8.0"

@[default_target]
lean_lib «CatNF» where
  -- Add library configuration options here

lean_exe «bench» where
  root := `bench.Bench
  supportInterpreter := true

lean_exe «test-runner» where
  root := `tests.TestRunner
  supportInterpreter := true

lean_exe «test-runner-final» where
  root := `test_runner_final
  supportInterpreter := true

-- Test configuration

-- Documentation target
target «docs» : Unit := do
  proc {
    cmd := "lake"
    args := #["exe", "doc-gen4", "--", "CatNF"]
  }
  return .nil
