import Lean.CoreM
import Lean.Environment
import Lean.Meta
import CatNF.Core

open Lean Core Meta CatNF
open CatNF.MorphismNames

/-- Lightweight alternate test entrypoint (smoke checks only). -/
unsafe def main : IO UInt32 := do
  Lean.initSearchPath (← Lean.findSysroot)
  withImportModules #[{ module := `CatNF.Core }] {} (fun env => do
    let coreCtx : Core.Context := { fileName := "<final>", fileMap := default }
    let coreS : Core.State := { env }
    let m : CoreM UInt32 := do
      let (ok, _) ← MetaM.run (do
        let f := mkConst (Name.mkSimple "f")
        let g := mkConst (Name.mkSimple "g")
        let comp := mkCategoryComp f g
        let b ← runCatNFM! (isComposition comp)
        unless b do throwError "composition check failed"
        let idE := mkCategoryId f
        let b2 ← runCatNFM! (isIdentity idE)
        unless b2 do throwError "identity check failed"
        let cfg : CatNF.Config := {
          maxSteps := 100
          timeoutMs := 2000
          monoidal := true
          trace := false
          simpSet := none
        }
        let _ ← normalizeGoalM comp cfg
        return true
      ) {} {}
      return if ok then 0 else 1
    let (code, _) ← CoreM.toIO m coreCtx coreS
    return code)
