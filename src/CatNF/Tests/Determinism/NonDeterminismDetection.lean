import Mathlib.CategoryTheory.Category.Basic
import Lean.Expr
import Lean.Meta
import CatNF.Core

namespace CatNF.Tests.Determinism.NonDeterminismDetection

open Lean Meta CatNF
open CatNF.MorphismNames

def smokeConfig : Config := {
  maxSteps := 50
  timeoutMs := 2000
  monoidal := true
  trace := false
  simpSet := none
}

/-- Placeholder non-determinism suite: same check as determinism smoke (extend when metrics exist). -/
def runAllNonDeterminismDetectionTests : MetaM Unit := do
  let f := mkConst (Name.mkSimple "f")
  let g := mkConst (Name.mkSimple "g")
  let comp := mkCategoryComp f g
  let _ ← normalizeGoalM comp smokeConfig
  return ()

end CatNF.Tests.Determinism.NonDeterminismDetection
