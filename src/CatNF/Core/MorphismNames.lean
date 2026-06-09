import Lean.Expr

open Lean

/-!
# Morphism constant names

Single source of truth for `CategoryTheory` head symbols used in expression
pattern matching and rebuilding. Keeps the kernel Mathlib-free while tracking
Mathlib 4.31 naming (`CategoryTheory.Functor.whiskerLeft` / `whiskerRight`).
-/

namespace CatNF.MorphismNames

/-- Composition `CategoryTheory.CategoryStruct.comp`. -/
def categoryComp : Name := `CategoryTheory.CategoryStruct.comp

/-- Identity `CategoryTheory.CategoryStruct.id`. -/
def categoryId : Name := `CategoryTheory.CategoryStruct.id

/-- Isomorphism forward map `CategoryTheory.Iso.hom`. -/
def isoHom : Name := `CategoryTheory.Iso.hom

/-- Isomorphism inverse map `CategoryTheory.Iso.inv`. -/
def isoInv : Name := `CategoryTheory.Iso.inv

/-- Isomorphism composition `CategoryTheory.Iso.trans`. -/
def isoTrans : Name := `CategoryTheory.Iso.trans

/-- Functor action on morphisms `CategoryTheory.Functor.map`. -/
def functorMap : Name := `CategoryTheory.Functor.map

/-- Functor action on isomorphisms `CategoryTheory.Functor.mapIso`. -/
def functorMapIso : Name := `CategoryTheory.Functor.mapIso

/-- Left whiskering `CategoryTheory.Functor.whiskerLeft`. -/
def whiskerLeft : Name := `CategoryTheory.Functor.whiskerLeft

/-- Right whiskering `CategoryTheory.Functor.whiskerRight`. -/
def whiskerRight : Name := `CategoryTheory.Functor.whiskerRight

/-- Monoidal tensor on objects/morphisms `CategoryTheory.MonoidalCategory.tensorObj`. -/
def tensorObj : Name := `CategoryTheory.MonoidalCategory.tensorObj

/-- Monoidal associator `CategoryTheory.MonoidalCategory.associator`. -/
def associator : Name := `CategoryTheory.MonoidalCategory.associator

/-- Monoidal left unitor `CategoryTheory.MonoidalCategory.leftUnitor`. -/
def leftUnitor : Name := `CategoryTheory.MonoidalCategory.leftUnitor

/-- Monoidal right unitor `CategoryTheory.MonoidalCategory.rightUnitor`. -/
def rightUnitor : Name := `CategoryTheory.MonoidalCategory.rightUnitor

/-- Braiding natural transformation `CategoryTheory.MonoidalCategory.braiding`. -/
def braiding : Name := `CategoryTheory.MonoidalCategory.braiding

/-- `comp f g` when the head is `categoryComp`. -/
def asCategoryComp? (e : Expr) : Option (Expr × Expr) :=
  match e with
  | .app (.app (.const n _) f) g => if n == categoryComp then some (f, g) else none
  | _ => none

/-- `id C` when the head is `categoryId`. -/
def asCategoryId? (e : Expr) : Option Expr :=
  match e with
  | .app (.const n _) arg => if n == categoryId then some arg else none
  | _ => none

/-- `Iso.hom iso`. -/
def asIsoHom? (e : Expr) : Option Expr :=
  match e with
  | .app (.const n _) iso => if n == isoHom then some iso else none
  | _ => none

/-- `Iso.inv iso`. -/
def asIsoInv? (e : Expr) : Option Expr :=
  match e with
  | .app (.const n _) iso => if n == isoInv then some iso else none
  | _ => none

/-- `Functor.map F f`. -/
def asFunctorMap? (e : Expr) : Option (Expr × Expr) :=
  match e with
  | .app (.app (.const n _) F) f => if n == functorMap then some (F, f) else none
  | _ => none

/-- `Functor.whiskerLeft F α`. -/
def asWhiskerLeft? (e : Expr) : Option (Expr × Expr) :=
  match e with
  | .app (.app (.const n _) F) f => if n == whiskerLeft then some (F, f) else none
  | _ => none

/-- `Functor.whiskerRight α G`. -/
def asWhiskerRight? (e : Expr) : Option (Expr × Expr) :=
  match e with
  | .app (.app (.const n _) f) G => if n == whiskerRight then some (f, G) else none
  | _ => none

/-- `MonoidalCategory.tensorObj f g`. -/
def asTensorObj? (e : Expr) : Option (Expr × Expr) :=
  match e with
  | .app (.app (.const n _) f) g => if n == tensorObj then some (f, g) else none
  | _ => none

/-- `MonoidalCategory.associator f g h`. -/
def asAssociator? (e : Expr) : Option (Expr × Expr × Expr) :=
  match e with
  | .app (.app (.app (.const n _) f) g) h => if n == associator then some (f, g, h) else none
  | _ => none

/-- `MonoidalCategory.leftUnitor f _`. -/
def asLeftUnitor? (e : Expr) : Option Expr :=
  match e with
  | .app (.app (.const n _) f) _ => if n == leftUnitor then some f else none
  | _ => none

/-- `MonoidalCategory.rightUnitor f _`. -/
def asRightUnitor? (e : Expr) : Option Expr :=
  match e with
  | .app (.app (.const n _) f) _ => if n == rightUnitor then some f else none
  | _ => none

/-- `MonoidalCategory.braiding f g`. -/
def asBraiding? (e : Expr) : Option (Expr × Expr) :=
  match e with
  | .app (.app (.const n _) f) g => if n == braiding then some (f, g) else none
  | _ => none

def mkCategoryComp (f g : Expr) : Expr :=
  mkApp2 (mkConst categoryComp) f g

def mkCategoryId (obj : Expr) : Expr :=
  mkApp (mkConst categoryId) obj

def mkIsoHom (iso : Expr) : Expr :=
  mkApp (mkConst isoHom) iso

def mkIsoInv (iso : Expr) : Expr :=
  mkApp (mkConst isoInv) iso

def mkFunctorMap (F f : Expr) : Expr :=
  mkApp2 (mkConst functorMap) F f

def mkWhiskerLeft (F f : Expr) : Expr :=
  mkApp2 (mkConst whiskerLeft) F f

def mkWhiskerRight (f G : Expr) : Expr :=
  mkApp2 (mkConst whiskerRight) f G

def mkTensorObj (f g : Expr) : Expr :=
  mkApp2 (mkConst tensorObj) f g

end CatNF.MorphismNames
