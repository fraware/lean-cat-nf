import Mathlib.CategoryTheory.Category.Basic
import Mathlib.CategoryTheory.Functor.Basic
import Mathlib.CategoryTheory.Iso
import Mathlib.CategoryTheory.Monoidal.Category
import Mathlib.CategoryTheory.Monoidal.Braided
import Mathlib.CategoryTheory.Monoidal.Symmetric
import Mathlib.Data.List.Basic
import Mathlib.Data.Array.Basic
import Lean.Expr
import Lean.Meta
import Lean.Elab.Command
import Mathlib.Tactic.Basic
import Mathlib.Tactic.SimpRw

namespace CatNF

-- Attribute system for category normal form

-- Attribute for isomorphism rules
attribute [cat_nf.iso] CategoryTheory.Iso.refl
attribute [cat_nf.iso] CategoryTheory.Iso.symm
attribute [cat_nf.iso] CategoryTheory.Iso.trans

-- Attribute for functor rules
attribute [cat_nf.functor] CategoryTheory.Functor.map_id
attribute [cat_nf.functor] CategoryTheory.Functor.map_comp

-- Attribute for monoidal rules
attribute [cat_nf.monoidal] CategoryTheory.MonoidalCategory.tensor_id
attribute [cat_nf.monoidal] CategoryTheory.MonoidalCategory.id_tensor
attribute [cat_nf.monoidal] CategoryTheory.MonoidalCategory.tensor_comp

-- Attribute for whiskering rules
attribute [cat_nf.whisker] CategoryTheory.whiskerLeft_id
attribute [cat_nf.whisker] CategoryTheory.whiskerRight_id
attribute [cat_nf.whisker] CategoryTheory.whiskerLeft_comp
attribute [cat_nf.whisker] CategoryTheory.whiskerRight_comp

-- Attribute for associativity rules
attribute [cat_nf.assoc] CategoryTheory.Category.assoc
attribute [cat_nf.assoc] CategoryTheory.Category.id_comp
attribute [cat_nf.assoc] CategoryTheory.Category.comp_id

-- Attribute for unit rules
attribute [cat_nf.unit] CategoryTheory.Category.id_comp
attribute [cat_nf.unit] CategoryTheory.Category.comp_id

-- Attribute for coherence rules
attribute [cat_nf.coherence] CategoryTheory.MonoidalCategory.pentagon
attribute [cat_nf.coherence] CategoryTheory.MonoidalCategory.triangle

-- Attribute for braiding rules
attribute [cat_nf.braid] CategoryTheory.MonoidalCategory.braiding_naturality
attribute [cat_nf.braid] CategoryTheory.MonoidalCategory.braiding_tensor

-- Attribute for symmetry rules
attribute [cat_nf.symmetry] CategoryTheory.MonoidalCategory.braiding_symm

-- Attribute for custom rules
attribute [cat_nf.custom] CategoryTheory.Iso.hom_inv_id
attribute [cat_nf.custom] CategoryTheory.Iso.inv_hom_id

-- Attribute for unsafe rules
attribute [cat_nf.unsafe] CategoryTheory.Iso.hom_inv_id
attribute [cat_nf.unsafe] CategoryTheory.Iso.inv_hom_id

-- Attribute for high priority rules
attribute [cat_nf.priority] CategoryTheory.Category.assoc
attribute [cat_nf.priority] CategoryTheory.Category.id_comp
attribute [cat_nf.priority] CategoryTheory.Category.comp_id

-- Attribute for low priority rules
attribute [cat_nf.low_priority] CategoryTheory.MonoidalCategory.pentagon
attribute [cat_nf.low_priority] CategoryTheory.MonoidalCategory.triangle

-- Attribute for experimental rules
attribute [cat_nf.experimental] CategoryTheory.MonoidalCategory.braiding_hexagon_forward
attribute [cat_nf.experimental] CategoryTheory.MonoidalCategory.braiding_hexagon_reverse

-- Attribute for deprecated rules
attribute [cat_nf.deprecated] CategoryTheory.MonoidalCategory.braiding_hexagon_forward
attribute [cat_nf.deprecated] CategoryTheory.MonoidalCategory.braiding_hexagon_reverse

-- Attribute for performance critical rules
attribute [cat_nf.performance] CategoryTheory.Category.assoc
attribute [cat_nf.performance] CategoryTheory.Category.id_comp
attribute [cat_nf.performance] CategoryTheory.Category.comp_id

-- Attribute for memory intensive rules
attribute [cat_nf.memory] CategoryTheory.MonoidalCategory.pentagon
attribute [cat_nf.memory] CategoryTheory.MonoidalCategory.triangle

-- Attribute for time intensive rules
attribute [cat_nf.time] CategoryTheory.MonoidalCategory.braiding_hexagon_forward
attribute [cat_nf.time] CategoryTheory.MonoidalCategory.braiding_hexagon_reverse

-- Attribute for debugging rules
attribute [cat_nf.debug] CategoryTheory.Iso.hom_inv_id
attribute [cat_nf.debug] CategoryTheory.Iso.inv_hom_id

-- Attribute for testing rules
attribute [cat_nf.test] CategoryTheory.Iso.refl
attribute [cat_nf.test] CategoryTheory.Iso.symm
attribute [cat_nf.test] CategoryTheory.Iso.trans

-- Attribute for production rules
attribute [cat_nf.production] CategoryTheory.Category.assoc
attribute [cat_nf.production] CategoryTheory.Category.id_comp
attribute [cat_nf.production] CategoryTheory.Category.comp_id

-- Attribute for development rules
attribute [cat_nf.development] CategoryTheory.MonoidalCategory.pentagon
attribute [cat_nf.development] CategoryTheory.MonoidalCategory.triangle

-- Attribute for staging rules
attribute [cat_nf.staging] CategoryTheory.MonoidalCategory.braiding_hexagon_forward
attribute [cat_nf.staging] CategoryTheory.MonoidalCategory.braiding_hexagon_reverse

end CatNF
