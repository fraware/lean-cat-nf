import Mathlib.CategoryTheory.Category.Basic
import Lean.Expr
import Lean.Meta
import CatNF.Core
import CatNF.Monoidal.Core

namespace CatNF.Tests.Unit.Monoidal

open Lean Meta CatNF CatNF.Monoidal

def runAllTests : MetaM Unit := do
  let f := mkConst (Name.mkSimple "f")
  let g := mkConst (Name.mkSimple "g")
  let segs : List ExprSegment := [ExprSegment.raw f, ExprSegment.raw g]
  let out ← applyAssociators segs
  assert! out.length ≥ 1
  logInfo "Monoidal unit smoke test passed."

end CatNF.Tests.Unit.Monoidal
