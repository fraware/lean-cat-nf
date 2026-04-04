import Lean.Expr
import Lean.Meta
import CatNF.RewriteRules

open Lean Meta

namespace CatNF

/-- Shape used by tests for mock declaration metadata. -/
structure IsoDeclPreview where
  name : Name
  type : Expr
  value : Expr
  levelParams : List Name := []
  all : List Name := []
  isUnsafe : Bool := false
  isPartial : Bool := false
  isNonComputable : Bool := false

/-- Returns `true` when the declaration type is an `Eq` application (isomorphism-style). -/
def validateIsoDeclaration (decl : IsoDeclPreview) : MetaM Bool :=
  return decl.type.isAppOf ``Eq

/-- Returns `true` if `nm` is declared in the current environment. -/
def validateRule (nm : Name) : MetaM Bool := do
  let env ← getEnv
  return env.contains nm

/-- Derive a `RewriteSchema` from an iso-style declaration name (used by tests and tooling). -/
def extractRewriteSchema (decl : IsoDeclPreview) : MetaM RewriteSchema := do
  let n := decl.name
  let base := n.toString (escape := false)
  return {
    homRule := n
    invRule := Name.mkSimple (base ++ "inv")
    homInvId := Name.mkSimple (base ++ "hom_inv_id")
    invHomId := Name.mkSimple (base ++ "inv_hom_id")
  }

end CatNF
