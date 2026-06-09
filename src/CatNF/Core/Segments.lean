import Lean.Expr
import Lean.Meta
import CatNF.Core.Config
import CatNF.Core.Error

open Lean Meta

namespace CatNF

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

private def exprEq (a b : Expr) : Bool :=
  a == b

private def beqImpl : ExprSegment → ExprSegment → Bool
  | ExprSegment.id, ExprSegment.id => true
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

def validateExprSegment (seg : ExprSegment) : CatNFM Unit := do
  match seg with
  | .comp f g => do
    validateExprSegment f
    validateExprSegment g
  | .id => return ()
  | .iso_hom iso =>
    if iso.isMVar then
      throwCatNF (CatNFError.validationError "isomorphism expression cannot be a metavariable")
  | .iso_inv iso =>
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
  | .raw expr =>
    if expr.isMVar then
      throwCatNF (CatNFError.validationError "raw expression cannot be a metavariable")

def validateExprSegments (segs : List ExprSegment) : CatNFM Unit := do
  if segs.isEmpty then
    throwCatNF (CatNFError.validationError "expression segment list cannot be empty")
  if segs.length > 1000 then
    throwCatNF (CatNFError.validationError "expression segment list too long (max 1000 segments)")
  for seg in segs do
    validateExprSegment seg

/-- Applied rewrite record for comprehensive tracing and debugging -/
structure AppliedRewrite where
  rule : String
  before : Expr
  after : Expr
  step : Nat
  timestamp : Nat := 0

def validateAppliedRewrite (appRw : AppliedRewrite) : CatNFM Unit := do
  if appRw.rule.isEmpty then
    throwCatNF (CatNFError.validationError "rewrite rule name cannot be empty")
  if appRw.rule.length > 200 then
    throwCatNF (CatNFError.validationError "rewrite rule name too long (max 200 characters)")
  if appRw.step == 0 then
    throwCatNF (CatNFError.validationError "rewrite step must be greater than 0")

/-- Normal form state with comprehensive tracking and validation -/
structure NFState where
  segments : List ExprSegment
  rewrites : List AppliedRewrite
  steps : Nat := 0
  config : Config
  startTime : Nat := 0

def validateNFState (state : NFState) : CatNFM Unit := do
  validateConfig state.config
  validateExprSegments state.segments
  if state.steps > state.config.maxSteps then
    throwCatNF (CatNFError.validationError s!"step count ({state.steps}) exceeds maxSteps ({state.config.maxSteps})")
  for appRw in state.rewrites do
    validateAppliedRewrite appRw

def createNFState (segments : List ExprSegment) (config : Config) : CatNFM NFState := do
  validateConfig config
  validateExprSegments segments
  return NFState.mk segments [] 0 config 0

end CatNF
