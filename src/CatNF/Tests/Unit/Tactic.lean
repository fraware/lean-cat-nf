import Mathlib.CategoryTheory.Category.Basic
import Lean.Expr
import Lean.Meta
import CatNF.Core

namespace CatNF.Tests.Unit.Tactic

open Lean Meta CatNF
open CatNF.MorphismNames

def testConfig : Config := {
  maxSteps := 100
  timeoutMs := 1000
  monoidal := true
  trace := false
  simpSet := none
}

def runAllTests : MetaM Unit := do
  let f := mkConst (Name.mkSimple "f")
  let g := mkConst (Name.mkSimple "g")
  let comp := mkCategoryComp f g
  let b ← runCatNFM! (isComposition comp)
  assert! b
  let b2 ← runCatNFM! (isIdentity (mkCategoryId f))
  assert! b2
  logInfo "Tactic unit smoke test passed."

end CatNF.Tests.Unit.Tactic
