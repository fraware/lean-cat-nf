import Mathlib.CategoryTheory.Category.Basic
import Lean.Expr
import Lean.Meta
import Mathlib.Tactic.Basic
import CatNF.Core

namespace CatNF.Tests.Performance.RegressionDetection

open Lean Meta CatNF

def regConfig : Config := {
  maxSteps := 100
  timeoutMs := 5000
  monoidal := true
  trace := false
  simpSet := none
}

def runAllRegressionTests : MetaM Unit := do
  let f := mkConst (Name.mkSimple "f")
  let g := mkConst (Name.mkSimple "g")
  let comp := mkApp2 (mkConst `CategoryTheory.CategoryStruct.comp) f g
  let _ ← normalizeGoalM comp regConfig
  return ()

end CatNF.Tests.Performance.RegressionDetection
