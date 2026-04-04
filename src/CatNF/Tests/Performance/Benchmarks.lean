import Mathlib.CategoryTheory.Category.Basic
import Lean.Expr
import Lean.Meta
import Mathlib.Tactic.Basic
import CatNF.Core

namespace CatNF.Tests.Performance

open Lean Meta CatNF

def benchConfig : Config := {
  maxSteps := 100
  timeoutMs := 5000
  monoidal := true
  trace := false
  simpSet := none
}

def runPerformanceTests : MetaM Unit := do
  let f := mkConst (Name.mkSimple "f")
  let g := mkConst (Name.mkSimple "g")
  let comp := mkApp2 (mkConst `CategoryTheory.CategoryStruct.comp) f g
  let _ ← normalizeGoalM comp benchConfig
  return ()

end CatNF.Tests.Performance
