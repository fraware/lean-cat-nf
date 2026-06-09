import Lean.Meta

open Lean Meta

namespace CatNF

/-- Custom exception types for comprehensive error handling -/
inductive CatNFError where
  | timeoutError (message : String) : CatNFError
  | validationError (message : String) : CatNFError
  | normalizationError (message : String) : CatNFError
  | configurationError (message : String) : CatNFError
  | internalError (message : String) : CatNFError

instance : ToString CatNFError where
  toString err := match err with
    | .timeoutError msg => s!"CatNF Timeout Error: {msg}"
    | .validationError msg => s!"CatNF Validation Error: {msg}"
    | .normalizationError msg => s!"CatNF Normalization Error: {msg}"
    | .configurationError msg => s!"CatNF Configuration Error: {msg}"
    | .internalError msg => s!"CatNF Internal Error: {msg}"

/-- Exception handling monad transformer for CatNF operations -/
abbrev CatNFM (α : Type) := ExceptT CatNFError MetaM α

/-- Disambiguate `throw` from `MetaM`'s `MonadExceptOf Exception` when stacked with `ExceptT`. -/
@[inline] def throwCatNF {α : Type} (e : CatNFError) : CatNFM α :=
  throwThe CatNFError e

/-- Lift MetaM operations to CatNFM (Lean `Exception` still aborts via inner `MetaM`). -/
def liftMetaM {α : Type} (action : MetaM α) : CatNFM α :=
  ExceptT.lift action

/-- Run `CatNFM` in `MetaM`, turning `CatNFError` into a Lean exception. -/
def runCatNFM! {α : Type} (m : CatNFM α) : MetaM α := do
  match (← m.run) with
  | .ok a => return a
  | .error e => throwError (toString e)

end CatNF
