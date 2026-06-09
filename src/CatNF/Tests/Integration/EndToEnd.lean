import Mathlib.CategoryTheory.Category.Basic
import Lean.Expr
import Lean.Meta
import CatNF.Core

namespace CatNF.Tests.Integration.EndToEnd

open Lean Meta CatNF
open CatNF.MorphismNames

def testConfig : Config := {
  maxSteps := 100
  timeoutMs := 2000
  monoidal := true
  trace := false
  simpSet := none
}

def runAllTests : MetaM Unit := do
  let f := mkConst (Name.mkSimple "f")
  let g := mkConst (Name.mkSimple "g")
  let comp := mkCategoryComp f g
  let _ ← normalizeGoalM comp testConfig
  logInfo "End-to-end smoke test passed."

end CatNF.Tests.Integration.EndToEnd
