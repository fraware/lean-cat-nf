import Mathlib.CategoryTheory.Category.Basic
import Lean.Expr
import Lean.Meta
import CatNF.Core

namespace CatNF.Tests.Determinism

open Lean Meta CatNF
open CatNF.MorphismNames

def smokeConfig : Config := {
  maxSteps := 50
  timeoutMs := 2000
  monoidal := true
  trace := false
  simpSet := none
}

/-- Lightweight determinism smoke test: repeated `normalizeGoalM` on the same expression. -/
def runAllDeterminismTests : MetaM Unit := do
  let f := mkConst (Name.mkSimple "f")
  let g := mkConst (Name.mkSimple "g")
  let comp := mkCategoryComp f g
  let (a, _) ← normalizeGoalM comp smokeConfig
  let (b, _) ← normalizeGoalM comp smokeConfig
  unless a == b do
    throwError "determinism smoke: normalizeGoalM results differed between runs"
  return ()

end CatNF.Tests.Determinism
