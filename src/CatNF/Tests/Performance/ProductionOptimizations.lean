import Mathlib.CategoryTheory.Category.Basic
import Lean.Expr
import Lean.Meta
import Mathlib.Tactic.Basic
import CatNF.Core

namespace CatNF.Performance

open Lean Meta CatNF

def smokeConfig : Config := {
  maxSteps := 100
  timeoutMs := 2000
  monoidal := true
  trace := false
  simpSet := none
}

/-- Smoke benchmark: normalization completes on a simple composition. -/
def runProductionSmoke : MetaM Unit := do
  let f := mkConst (Name.mkSimple "f")
  let g := mkConst (Name.mkSimple "g")
  let comp := mkApp2 (mkConst `CategoryTheory.CategoryStruct.comp) f g
  let _ ← normalizeGoalM comp smokeConfig
  logInfo "Production performance smoke test passed."

end CatNF.Performance
