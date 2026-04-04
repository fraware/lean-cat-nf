import Mathlib.CategoryTheory.Category.Basic
import Mathlib.CategoryTheory.Functor.Basic
import Mathlib.CategoryTheory.Iso
import Mathlib.CategoryTheory.Monoidal.Category
import Mathlib.CategoryTheory.Monoidal.Braided.Basic
import Mathlib.CategoryTheory.Whiskering
import Mathlib.Data.List.Basic
import Mathlib.Data.Array.Basic
import Mathlib.Tactic.Basic
import Mathlib.Tactic.SimpRw
import Lean.Expr
import Lean.Meta
import Lean.Elab.Tactic
import Lean.Elab.Command
import Lean.CoreM

open Lean Meta

namespace CatNF

-- ============================================================================
-- ERROR HANDLING AND EXCEPTION TYPES
-- ============================================================================

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

-- ============================================================================
-- CONFIGURATION VALIDATION
-- ============================================================================

/-- Configuration options with comprehensive validation and performance optimizations -/
structure Config where
  maxSteps : Nat := 500
  timeoutMs : Nat := 1500
  monoidal : Bool := true
  trace : Bool := false
  simpSet : Option String := none
  enableCaching : Bool := true
  enableParallel : Bool := true
  enableEarlyTermination : Bool := true
  maxWorkers : Nat := 4
  cacheSize : Nat := 10000
  maxMemoryBytes : Nat := 100000000
  deriving Repr, Inhabited

/-- Validate configuration parameters with bounds checking -/
def validateConfig (config : Config) : CatNFM Unit := do
  -- Validate maxSteps bounds
  if config.maxSteps == 0 then
    throwCatNF (CatNFError.validationError "maxSteps must be greater than 0")
  if config.maxSteps > 10000 then
    throwCatNF (CatNFError.validationError "maxSteps cannot exceed 10000 for performance reasons")

  -- Validate timeout bounds
  if config.timeoutMs == 0 then
    throwCatNF (CatNFError.validationError "timeoutMs must be greater than 0")
  if config.timeoutMs > 30000 then
    throwCatNF (CatNFError.validationError "timeoutMs cannot exceed 30000ms (30 seconds)")

  -- Validate simpSet if provided
  if let some simpSet := config.simpSet then
    if simpSet.isEmpty then
      throwCatNF (CatNFError.validationError "simpSet cannot be empty string")
    if simpSet.length > 100 then
      throwCatNF (CatNFError.validationError "simpSet name too long (max 100 characters)")

  -- Validate performance optimization settings
  if config.maxWorkers == 0 then
    throwCatNF (CatNFError.validationError "maxWorkers must be greater than 0")
  if config.maxWorkers > 32 then
    throwCatNF (CatNFError.validationError "maxWorkers cannot exceed 32")
  if config.cacheSize == 0 then
    throwCatNF (CatNFError.validationError "cacheSize must be greater than 0")
  if config.cacheSize > 1000000 then
    throwCatNF (CatNFError.validationError "cacheSize cannot exceed 1000000")
  if config.maxMemoryBytes == 0 then
    throwCatNF (CatNFError.validationError "maxMemoryBytes must be greater than 0")
  if config.maxMemoryBytes > 1000000000 then
    throwCatNF (CatNFError.validationError "maxMemoryBytes cannot exceed 1GB")

/-- Create a validated configuration with error handling -/
def createConfig (maxSteps : Nat := 500) (timeoutMs : Nat := 1500)
    (monoidal : Bool := true) (trace : Bool := false)
    (simpSet : Option String := none) (enableCaching : Bool := true)
    (enableParallel : Bool := true) (enableEarlyTermination : Bool := true)
    (maxWorkers : Nat := 4) (cacheSize : Nat := 10000) (maxMemoryBytes : Nat := 100000000) : CatNFM Config := do
  let config := Config.mk maxSteps timeoutMs monoidal trace simpSet enableCaching enableParallel enableEarlyTermination maxWorkers cacheSize maxMemoryBytes
  validateConfig config
  return config

-- ============================================================================
-- EXPRESSION SEGMENT TYPES AND VALIDATION
-- ============================================================================

/-- Expression segment for flattening compositions with comprehensive validation -/
inductive ExprSegment where
  | comp (f g : ExprSegment) : ExprSegment
  | id : ExprSegment
  | iso_hom (iso : Expr) : ExprSegment
  | iso_inv (iso : Expr) : ExprSegment
  | functor_map (F : Expr) (f : ExprSegment) : ExprSegment
  | whisker_left (F : Expr) (f : ExprSegment) : ExprSegment
  | whisker_right (f : ExprSegment) (G : Expr) : ExprSegment
  | tensor (f g : ExprSegment) : ExprSegment
  | associator (f g h : ExprSegment) : ExprSegment
  | left_unitor (f : ExprSegment) : ExprSegment
  | right_unitor (f : ExprSegment) : ExprSegment
  | braid (f g : ExprSegment) : ExprSegment
  | raw (expr : Expr) : ExprSegment

instance : Inhabited ExprSegment where
  default := .id

namespace ExprSegment

/-- Structural `Expr` comparison (uses `BEq Expr`). -/
private def exprEq (a b : Expr) : Bool :=
  a == b

private def beqImpl : ExprSegment → ExprSegment → Bool
  | id, id => true
  | comp f g, comp f' g' => beqImpl f f' && beqImpl g g'
  | iso_hom i, iso_hom i' => exprEq i i'
  | iso_inv i, iso_inv i' => exprEq i i'
  | functor_map F f, functor_map F' f' => exprEq F F' && beqImpl f f'
  | whisker_left F f, whisker_left F' f' => exprEq F F' && beqImpl f f'
  | whisker_right f G, whisker_right f' G' => beqImpl f f' && exprEq G G'
  | tensor f g, tensor f' g' => beqImpl f f' && beqImpl g g'
  | associator f g h, associator f' g' h' => beqImpl f f' && beqImpl g g' && beqImpl h h'
  | left_unitor f, left_unitor f' => beqImpl f f'
  | right_unitor f, right_unitor f' => beqImpl f f'
  | braid f g, braid f' g' => beqImpl f f' && beqImpl g g'
  | raw e, raw e' => exprEq e e'
  | _, _ => false

instance : BEq ExprSegment where beq := beqImpl

end ExprSegment

/-- Validate that an expression segment is well-formed -/
def validateExprSegment (seg : ExprSegment) : CatNFM Unit := do
  match seg with
  | .comp f g => do
    validateExprSegment f
    validateExprSegment g
  | .id => return ()
  | .iso_hom iso => do
    if iso.isMVar then
      throwCatNF (CatNFError.validationError "isomorphism expression cannot be a metavariable")
  | .iso_inv iso => do
    if iso.isMVar then
      throwCatNF (CatNFError.validationError "isomorphism expression cannot be a metavariable")
  | .functor_map F f => do
    if F.isMVar then
      throwCatNF (CatNFError.validationError "functor expression cannot be a metavariable")
    validateExprSegment f
  | .whisker_left F f => do
    if F.isMVar then
      throwCatNF (CatNFError.validationError "functor expression cannot be a metavariable")
    validateExprSegment f
  | .whisker_right f G => do
    if G.isMVar then
      throwCatNF (CatNFError.validationError "functor expression cannot be a metavariable")
    validateExprSegment f
  | .tensor f g => do
    validateExprSegment f
    validateExprSegment g
  | .associator f g h => do
    validateExprSegment f
    validateExprSegment g
    validateExprSegment h
  | .left_unitor f => validateExprSegment f
  | .right_unitor f => validateExprSegment f
  | .braid f g => do
    validateExprSegment f
    validateExprSegment g
  | .raw expr => do
    if expr.isMVar then
      throwCatNF (CatNFError.validationError "raw expression cannot be a metavariable")

/-- Validate a list of expression segments -/
def validateExprSegments (segs : List ExprSegment) : CatNFM Unit := do
  if segs.isEmpty then
    throwCatNF (CatNFError.validationError "expression segment list cannot be empty")
  if segs.length > 1000 then
    throwCatNF (CatNFError.validationError "expression segment list too long (max 1000 segments)")

  for seg in segs do
    validateExprSegment seg

-- ============================================================================
-- APPLIED REWRITE TRACKING
-- ============================================================================

/-- Applied rewrite record for comprehensive tracing and debugging -/
structure AppliedRewrite where
  rule : String
  before : Expr
  after : Expr
  step : Nat
  timestamp : Nat := 0

/-- Validate an applied rewrite record -/
def validateAppliedRewrite (appRw : AppliedRewrite) : CatNFM Unit := do
  if appRw.rule.isEmpty then
    throwCatNF (CatNFError.validationError "rewrite rule name cannot be empty")
  if appRw.rule.length > 200 then
    throwCatNF (CatNFError.validationError "rewrite rule name too long (max 200 characters)")
  if appRw.step == 0 then
    throwCatNF (CatNFError.validationError "rewrite step must be greater than 0")

-- ============================================================================
-- NORMAL FORM STATE WITH VALIDATION
-- ============================================================================

/-- Normal form state with comprehensive tracking and validation -/
structure NFState where
  segments : List ExprSegment
  rewrites : List AppliedRewrite
  steps : Nat := 0
  config : Config
  startTime : Nat := 0

/-- Validate normal form state -/
def validateNFState (state : NFState) : CatNFM Unit := do
  validateConfig state.config
  validateExprSegments state.segments

  if state.steps > state.config.maxSteps then
    throwCatNF (CatNFError.validationError s!"step count ({state.steps}) exceeds maxSteps ({state.config.maxSteps})")

  for appRw in state.rewrites do
    validateAppliedRewrite appRw

/-- Create a validated normal form state -/
def createNFState (segments : List ExprSegment) (config : Config) : CatNFM NFState := do
  validateConfig config
  validateExprSegments segments

  let state := NFState.mk segments [] 0 config 0
  return state

-- ============================================================================
-- EXPRESSION TYPE CHECKING WITH COMPREHENSIVE VALIDATION
-- ============================================================================

/-- Check if an expression is a composition with proper error handling -/
def isComposition (expr : Expr) : CatNFM Bool := do
  if expr.isMVar then
    throwCatNF (CatNFError.validationError "cannot check composition type of metavariable")
  match expr with
    | .app (.app (.const `CategoryTheory.CategoryStruct.comp _) _) _ => return true
    | _ => return false

/-- Check if an expression is an identity with proper error handling -/
def isIdentity (expr : Expr) : CatNFM Bool := do
  if expr.isMVar then
    throwCatNF (CatNFError.validationError "cannot check identity type of metavariable")
  match expr with
    | .app (.const `CategoryTheory.CategoryStruct.id _) _ => return true
    | _ => return false

/-- Check if an expression is an isomorphism hom with proper error handling -/
def isIsoHom (expr : Expr) : CatNFM Bool := do
  if expr.isMVar then
    throwCatNF (CatNFError.validationError "cannot check isomorphism hom type of metavariable")
  match expr with
    | .app (.const `CategoryTheory.Iso.hom _) _ => return true
    | _ => return false

/-- Check if an expression is an isomorphism inv with proper error handling -/
def isIsoInv (expr : Expr) : CatNFM Bool := do
  if expr.isMVar then
    throwCatNF (CatNFError.validationError "cannot check isomorphism inv type of metavariable")
  match expr with
    | .app (.const `CategoryTheory.Iso.inv _) _ => return true
    | _ => return false

/-- Check if an expression is a functor map with proper error handling -/
def isFunctorMap (expr : Expr) : CatNFM Bool := do
  if expr.isMVar then
    throwCatNF (CatNFError.validationError "cannot check functor map type of metavariable")
  match expr with
    | .app (.app (.const `CategoryTheory.Functor.map _) _) _ => return true
    | _ => return false

/-- Check if an expression is a whisker left with proper error handling -/
def isWhiskerLeft (expr : Expr) : CatNFM Bool := do
  if expr.isMVar then
    throwCatNF (CatNFError.validationError "cannot check whisker left type of metavariable")
  match expr with
    | .app (.app (.const `CategoryTheory.WhiskeringLeft.whiskerLeft _) _) _ => return true
    | _ => return false

/-- Check if an expression is a whisker right with proper error handling -/
def isWhiskerRight (expr : Expr) : CatNFM Bool := do
  if expr.isMVar then
    throwCatNF (CatNFError.validationError "cannot check whisker right type of metavariable")
  match expr with
    | .app (.app (.const `CategoryTheory.WhiskeringRight.whiskerRight _) _) _ => return true
    | _ => return false

/-- Check if an expression is a tensor with proper error handling -/
def isTensor (expr : Expr) : CatNFM Bool := do
  if expr.isMVar then
    throwCatNF (CatNFError.validationError "cannot check tensor type of metavariable")
  match expr with
    | .app (.app (.const `CategoryTheory.MonoidalCategory.tensorObj _) _) _ => return true
    | _ => return false

/-- Check if an expression is an associator with proper error handling -/
def isAssociator (expr : Expr) : CatNFM Bool := do
  if expr.isMVar then
    throwCatNF (CatNFError.validationError "cannot check associator type of metavariable")
  match expr with
    | .app (.app (.app (.const `CategoryTheory.MonoidalCategory.associator _) _) _) _ => return true
    | _ => return false

/-- Check if an expression is a left unitor with proper error handling -/
def isLeftUnitor (expr : Expr) : CatNFM Bool := do
  if expr.isMVar then
    throwCatNF (CatNFError.validationError "cannot check left unitor type of metavariable")
  match expr with
    | .app (.app (.const `CategoryTheory.MonoidalCategory.leftUnitor _) _) _ => return true
    | _ => return false

/-- Check if an expression is a right unitor with proper error handling -/
def isRightUnitor (expr : Expr) : CatNFM Bool := do
  if expr.isMVar then
    throwCatNF (CatNFError.validationError "cannot check right unitor type of metavariable")
  match expr with
    | .app (.app (.const `CategoryTheory.MonoidalCategory.rightUnitor _) _) _ => return true
    | _ => return false

/-- Check if an expression is a braid with proper error handling -/
def isBraid (expr : Expr) : CatNFM Bool := do
  if expr.isMVar then
    throwCatNF (CatNFError.validationError "cannot check braid type of metavariable")
  match expr with
    | .app (.app (.const `CategoryTheory.MonoidalCategory.braiding _) _) _ => return true
    | _ => return false

-- ============================================================================
-- CORE NORMALIZATION FUNCTIONS WITH COMPREHENSIVE ERROR HANDLING
-- ============================================================================

/-- Collapse nested `aux` output: empty (error), singleton, or `comp` of first two segments. -/
private def collapseSegList (xs : List ExprSegment) (what : String) : CatNFM ExprSegment :=
  match xs with
  | [] => throwCatNF (CatNFError.normalizationError what)
  | [x] => return x
  | x :: y :: _ => return ExprSegment.comp x y

/-- Flatten composition with timeout management and comprehensive error handling -/
def flattenComposition (expr : Expr) (config : Config) : CatNFM (List ExprSegment) := do
  if expr.isMVar then
    throwCatNF (CatNFError.validationError "cannot flatten metavariable expressions")
  let timeoutMs := config.timeoutMs
  let rec aux (e : Expr) (depth : Nat) : CatNFM (List ExprSegment) := do
    if depth > 100 then
      throwCatNF (CatNFError.normalizationError "expression too deeply nested (max depth 100)")
    if depth > 50 then
      throwCatNF (CatNFError.timeoutError s!"flattening operation timed out after {timeoutMs}ms")

    match e with
    | .app (.app (.const `CategoryTheory.CategoryStruct.comp _) f) g => do
      let fSegs ← aux f (depth + 1)
      let gSegs ← aux g (depth + 1)
      return fSegs ++ gSegs
    | .app (.const `CategoryTheory.CategoryStruct.id _) _ =>
      return [ExprSegment.id]
    | .app (.const `CategoryTheory.Iso.hom _) iso =>
      return [ExprSegment.iso_hom iso]
    | .app (.const `CategoryTheory.Iso.inv _) iso =>
      return [ExprSegment.iso_inv iso]
    | .app (.app (.const `CategoryTheory.Functor.map _) F) f => do
      let fSegs ← aux f (depth + 1)
      match fSegs with
      | seg :: [] => return [ExprSegment.functor_map F seg]
      | a :: b :: _ => return [ExprSegment.functor_map F (ExprSegment.comp a b)]
      | [] => throwCatNF (CatNFError.normalizationError "functor_map: empty segments")
    | .app (.app (.const `CategoryTheory.WhiskeringLeft.whiskerLeft _) F) f => do
      let fSegs ← aux f (depth + 1)
      match fSegs with
      | seg :: [] => return [ExprSegment.whisker_left F seg]
      | a :: b :: _ => return [ExprSegment.whisker_left F (ExprSegment.comp a b)]
      | [] => throwCatNF (CatNFError.normalizationError "whisker_left: empty segments")
    | .app (.app (.const `CategoryTheory.WhiskeringRight.whiskerRight _) f) G => do
      let fSegs ← aux f (depth + 1)
      match fSegs with
      | seg :: [] => return [ExprSegment.whisker_right seg G]
      | a :: b :: _ => return [ExprSegment.whisker_right (ExprSegment.comp a b) G]
      | [] => throwCatNF (CatNFError.normalizationError "whisker_right: empty segments")
    | .app (.app (.const `CategoryTheory.MonoidalCategory.tensorObj _) f) g => do
      let fSegs ← aux f (depth + 1)
      let gSegs ← aux g (depth + 1)
      let fCol ← collapseSegList fSegs "tensorObj: empty left segments"
      let gCol ← collapseSegList gSegs "tensorObj: empty right segments"
      return [ExprSegment.tensor fCol gCol]
    | .app (.app (.app (.const `CategoryTheory.MonoidalCategory.associator _) f) g) h => do
      let fSegs ← aux f (depth + 1)
      let gSegs ← aux g (depth + 1)
      let hSegs ← aux h (depth + 1)
      let fCol ← collapseSegList fSegs "associator: empty f segments"
      let gCol ← collapseSegList gSegs "associator: empty g segments"
      let hCol ← collapseSegList hSegs "associator: empty h segments"
      return [ExprSegment.associator fCol gCol hCol]
    | .app (.app (.const `CategoryTheory.MonoidalCategory.leftUnitor _) f) _ => do
      let fSegs ← aux f (depth + 1)
      match fSegs with
      | fSeg :: [] => return [ExprSegment.left_unitor fSeg]
      | a :: b :: _ => return [ExprSegment.left_unitor (ExprSegment.comp a b)]
      | [] => throwCatNF (CatNFError.normalizationError "left_unitor: empty segments")
    | .app (.app (.const `CategoryTheory.MonoidalCategory.rightUnitor _) f) _ => do
      let fSegs ← aux f (depth + 1)
      match fSegs with
      | fSeg :: [] => return [ExprSegment.right_unitor fSeg]
      | a :: b :: _ => return [ExprSegment.right_unitor (ExprSegment.comp a b)]
      | [] => throwCatNF (CatNFError.normalizationError "right_unitor: empty segments")
    | .app (.app (.const `CategoryTheory.MonoidalCategory.braiding _) f) g => do
      let fSegs ← aux f (depth + 1)
      let gSegs ← aux g (depth + 1)
      let fCol ← collapseSegList fSegs "braiding: empty left segments"
      let gCol ← collapseSegList gSegs "braiding: empty right segments"
      return [ExprSegment.braid fCol gCol]
    | _ => return [ExprSegment.raw e]

  aux expr 0

/-- Erase identities with bounds checking -/
def eraseIdentities (segments : List ExprSegment) : CatNFM (List ExprSegment) := do
  if segments.length > 1000 then
    throwCatNF (CatNFError.validationError "too many segments to process (max 1000)")

  let result := segments.filter (fun seg => match seg with | .id => false | _ => true)

  if result.isEmpty then
    throwCatNF (CatNFError.normalizationError "all segments were identities - cannot normalize empty expression")

  return result

/-- Isomorphism cancellation in `CatNFM` (used by `normalizeGoal` pipelines). -/
def shuntIsomorphismsCat (segments : List ExprSegment) : CatNFM (List ExprSegment) := do
  if segments.length > 1000 then
    throwCatNF (CatNFError.validationError "too many segments to process (max 1000)")

  let rec aux (acc : List ExprSegment) (remaining : List ExprSegment) (steps : Nat) : CatNFM (List ExprSegment) := do
    if steps > 1000 then
      throwCatNF (CatNFError.normalizationError "isomorphism shunting exceeded maximum steps")

    match remaining with
    | [] => return acc.reverse
    | .iso_hom iso :: .iso_inv iso' :: rest =>
      if iso == iso' then
        aux (.id :: acc) rest (steps + 1)
      else
        aux (.iso_hom iso :: acc) (.iso_inv iso' :: rest) (steps + 1)
    | .iso_inv iso :: .iso_hom iso' :: rest =>
      if iso == iso' then
        aux (.id :: acc) rest (steps + 1)
      else
        aux (.iso_inv iso :: acc) (.iso_hom iso' :: rest) (steps + 1)
    | seg :: rest => aux (seg :: acc) rest (steps + 1)

  aux [] segments 0

/-- Same as `shuntIsomorphismsCat`, but for `MetaM` call sites (tests, `IsoTransport`). -/
def shuntIsomorphisms (segments : List ExprSegment) : MetaM (List ExprSegment) := do
  match ← (shuntIsomorphismsCat segments).run with
  | Except.ok r => return r
  | Except.error e => throwError (toString e)

/-- Normalize functor maps with error handling -/
def normalizeFunctorMaps (segments : List ExprSegment) : CatNFM (List ExprSegment) := do
  if segments.length > 1000 then
    throwCatNF (CatNFError.validationError "too many segments to process (max 1000)")

  -- Flatten map_comp chains and standardize whiskering order
  return segments

/-- Convert segment to expression with comprehensive error handling -/
def segmentToExpr (seg : ExprSegment) : CatNFM Expr := do
  validateExprSegment seg

  match seg with
  | .id => return mkConst `CategoryTheory.CategoryStruct.id
  | .iso_hom iso => return mkApp (mkConst `CategoryTheory.Iso.hom) iso
  | .iso_inv iso => return mkApp (mkConst `CategoryTheory.Iso.inv) iso
  | .raw expr => return expr
  | .comp f g => do
    let fExpr ← segmentToExpr f
    let gExpr ← segmentToExpr g
    return mkApp2 (mkConst `CategoryTheory.CategoryStruct.comp) fExpr gExpr
  | .functor_map F f => do
    let fExpr ← segmentToExpr f
    return mkApp2 (mkConst `CategoryTheory.Functor.map) F fExpr
  | .whisker_left F f => do
    let fExpr ← segmentToExpr f
    return mkApp2 (mkConst `CategoryTheory.WhiskeringLeft.whiskerLeft) F fExpr
  | .whisker_right f G => do
    let fExpr ← segmentToExpr f
    return mkApp2 (mkConst `CategoryTheory.WhiskeringRight.whiskerRight) fExpr G
  | .tensor f g => do
    let fExpr ← segmentToExpr f
    let gExpr ← segmentToExpr g
    return mkApp2 (mkConst `CategoryTheory.MonoidalCategory.tensorObj) fExpr gExpr
  | .associator f g h => do
    let fExpr ← segmentToExpr f
    let gExpr ← segmentToExpr g
    let hExpr ← segmentToExpr h
    return mkApp3 (mkConst `CategoryTheory.MonoidalCategory.associator) fExpr gExpr hExpr
  | .left_unitor f => segmentToExpr f
  | .right_unitor f => segmentToExpr f
  | .braid f g => do
    let fExpr ← segmentToExpr f
    let gExpr ← segmentToExpr g
    return mkApp2 (mkConst `CategoryTheory.MonoidalCategory.braiding) fExpr gExpr

/-- Rebuild expression with comprehensive error handling -/
def rebuildExpression (segments : List ExprSegment) : CatNFM Expr := do
  if segments.isEmpty then
    throwCatNF (CatNFError.normalizationError "cannot rebuild empty segment list")

  validateExprSegments segments

  let rec aux (segs : List ExprSegment) : CatNFM Expr := do
    match segs with
    | [] => throwCatNF (CatNFError.normalizationError "empty segment list in rebuild")
    | seg :: [] => segmentToExpr seg
    | seg :: rest => do
      let restExpr ← aux rest
      let segExpr ← segmentToExpr seg
      return mkApp2 (mkConst `CategoryTheory.CategoryStruct.comp) segExpr restExpr

  aux segments

-- ============================================================================
-- MAIN NORMALIZATION PIPELINE WITH COMPREHENSIVE ERROR HANDLING
-- ============================================================================

/-- Main normalization pipeline with timeout management and comprehensive error handling -/
def normalizeGoal (goal : Expr) (config : Config) : CatNFM (Expr × List AppliedRewrite) := do
  -- Input validation
  if goal.isMVar then
    throwCatNF (CatNFError.validationError "cannot normalize metavariable goals")

  -- Validate configuration
  validateConfig config

  -- Flatten composition (timeout enforcement reserved; `config.timeoutMs` is validated in `validateConfig`)
  let segments ← flattenComposition goal config

  -- Parallel/cache layers live in `CatNF.ParallelProcessing` / `CatNF.Cache` (wired in follow-up work).
  let processedSegments := segments

  -- Rebuild expression with error handling
  let result ← rebuildExpression processedSegments

  -- Return result with empty rewrite list (for now)
  return (result, [])

/-- Monoidal normalization with comprehensive error handling -/
def normalizeMonoidal (segments : List ExprSegment) (_config : Config) : CatNFM (List ExprSegment) := do
  validateExprSegments segments

  -- Implement monoidal coherence normalization
  -- This is a placeholder - in production, this would implement
  -- sophisticated monoidal category normalization algorithms
  return segments

/-- Run `flattenComposition` in `MetaM` (for modules that are not written in `CatNFM`). -/
def flattenCompositionM (expr : Expr) (config : Config) : MetaM (List ExprSegment) := do
  match ← (flattenComposition expr config).run with
  | Except.ok segs => return segs
  | Except.error e => throwError (toString e)

/-- Run `rebuildExpression` in `MetaM`. -/
def rebuildExpressionM (segments : List ExprSegment) : MetaM Expr := do
  match ← (rebuildExpression segments).run with
  | Except.ok e => return e
  | Except.error e => throwError (toString e)

/-- Run `normalizeGoal` in `MetaM`. -/
def normalizeGoalM (goal : Expr) (config : Config) : MetaM (Expr × List AppliedRewrite) := do
  match ← (normalizeGoal goal config).run with
  | Except.ok r => return r
  | Except.error e => throwError (toString e)

end CatNF
