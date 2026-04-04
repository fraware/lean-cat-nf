import Mathlib.CategoryTheory.Category.Basic
import Lean.Expr
import Lean.Meta
import Mathlib.Tactic.Basic
import CatNF.Core

namespace CatNF.Tests.Unit.Tactic

open Lean Meta CatNF

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
  let comp := mkApp2 (mkConst `CategoryTheory.CategoryStruct.comp) f g
  let b ← runCatNFM! (isComposition comp)
  assert! b
  let b2 ← runCatNFM! (isIdentity (mkApp (mkConst `CategoryTheory.CategoryStruct.id) f))
  assert! b2
  logInfo "Tactic unit smoke test passed."

end CatNF.Tests.Unit.Tactic
