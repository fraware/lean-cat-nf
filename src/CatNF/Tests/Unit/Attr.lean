import Mathlib.CategoryTheory.Category.Basic
import Lean.Expr
import Lean.Meta
import Lean.Elab.Command
import CatNF.Core
import CatNF.Attr
import CatNF.RewriteRules

namespace CatNF.Tests.Unit.Attr

open Lean Meta CatNF

def mkTestExpr (name : String) : MetaM Expr :=
  return mkConst (Name.mkSimple name)

def testValidateIsoDeclaration : MetaM Unit := do
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"
  let eqType := mkApp2 (mkConst `Eq) f g
  let decl : IsoDeclPreview := {
    name := `testIso
    type := eqType
    value := mkConst `rfl
  }
  let ok ← validateIsoDeclaration decl
  assert! ok

def testExtractRewriteSchema : MetaM Unit := do
  let f ← mkTestExpr "f"
  let g ← mkTestExpr "g"
  let decl : IsoDeclPreview := {
    name := `testIso
    type := mkApp2 (mkConst `Eq) f g
    value := mkConst `rfl
  }
  let schema ← extractRewriteSchema decl
  assert! schema.homRule == `testIso

def testRegistryRoundtrip : MetaM Unit := do
  let rules ← runCatNFM! getRegisteredRules
  assert! rules.size > 0
  let ro ← runCatNFM! (findRule `CategoryTheory.Iso.refl)
  assert! ro.isSome

def runAllTests : MetaM Unit := do
  testValidateIsoDeclaration
  testExtractRewriteSchema
  testRegistryRoundtrip
  logInfo "Attr unit tests passed."

end CatNF.Tests.Unit.Attr
