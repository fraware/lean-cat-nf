import Lean.Expr
import Lean.Meta
import CatNF.Core.Segments
import CatNF.Core.MorphismNames

open Lean Meta
open CatNF.MorphismNames

namespace CatNF

/-- Check if an expression is a composition with proper error handling -/
def isComposition (expr : Expr) : CatNFM Bool := do
  if expr.isMVar then
    throwCatNF (CatNFError.validationError "cannot check composition type of metavariable")
  return asCategoryComp? expr |>.isSome

def isIdentity (expr : Expr) : CatNFM Bool := do
  if expr.isMVar then
    throwCatNF (CatNFError.validationError "cannot check identity type of metavariable")
  match expr with
  | .const n _ => return n == categoryId
  | _ => return asCategoryId? expr |>.isSome

def isIsoHom (expr : Expr) : CatNFM Bool := do
  if expr.isMVar then
    throwCatNF (CatNFError.validationError "cannot check isomorphism hom type of metavariable")
  return asIsoHom? expr |>.isSome

def isIsoInv (expr : Expr) : CatNFM Bool := do
  if expr.isMVar then
    throwCatNF (CatNFError.validationError "cannot check isomorphism inv type of metavariable")
  return asIsoInv? expr |>.isSome

def isFunctorMap (expr : Expr) : CatNFM Bool := do
  if expr.isMVar then
    throwCatNF (CatNFError.validationError "cannot check functor map type of metavariable")
  return asFunctorMap? expr |>.isSome

def isWhiskerLeft (expr : Expr) : CatNFM Bool := do
  if expr.isMVar then
    throwCatNF (CatNFError.validationError "cannot check whisker left type of metavariable")
  return asWhiskerLeft? expr |>.isSome

def isWhiskerRight (expr : Expr) : CatNFM Bool := do
  if expr.isMVar then
    throwCatNF (CatNFError.validationError "cannot check whisker right type of metavariable")
  return asWhiskerRight? expr |>.isSome

def isTensor (expr : Expr) : CatNFM Bool := do
  if expr.isMVar then
    throwCatNF (CatNFError.validationError "cannot check tensor type of metavariable")
  return asTensorObj? expr |>.isSome

def isAssociator (expr : Expr) : CatNFM Bool := do
  if expr.isMVar then
    throwCatNF (CatNFError.validationError "cannot check associator type of metavariable")
  return asAssociator? expr |>.isSome

def isLeftUnitor (expr : Expr) : CatNFM Bool := do
  if expr.isMVar then
    throwCatNF (CatNFError.validationError "cannot check left unitor type of metavariable")
  return asLeftUnitor? expr |>.isSome

def isRightUnitor (expr : Expr) : CatNFM Bool := do
  if expr.isMVar then
    throwCatNF (CatNFError.validationError "cannot check right unitor type of metavariable")
  return asRightUnitor? expr |>.isSome

def isBraid (expr : Expr) : CatNFM Bool := do
  if expr.isMVar then
    throwCatNF (CatNFError.validationError "cannot check braid type of metavariable")
  return asBraiding? expr |>.isSome

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
    | .app (.app (.const n _) f) g =>
      if n == categoryComp then
        let fSegs ← aux f (depth + 1)
        let gSegs ← aux g (depth + 1)
        return fSegs ++ gSegs
      else if n == functorMap then
        let fSegs ← aux g (depth + 1)
        match fSegs with
        | seg :: [] => return [ExprSegment.functor_map f seg]
        | a :: b :: _ => return [ExprSegment.functor_map f (ExprSegment.comp a b)]
        | [] => throwCatNF (CatNFError.normalizationError "functor_map: empty segments")
      else if n == whiskerLeft then
        let fSegs ← aux g (depth + 1)
        match fSegs with
        | seg :: [] => return [ExprSegment.whisker_left f seg]
        | a :: b :: _ => return [ExprSegment.whisker_left f (ExprSegment.comp a b)]
        | [] => throwCatNF (CatNFError.normalizationError "whisker_left: empty segments")
      else if n == whiskerRight then
        let fSegs ← aux f (depth + 1)
        match fSegs with
        | seg :: [] => return [ExprSegment.whisker_right seg g]
        | a :: b :: _ => return [ExprSegment.whisker_right (ExprSegment.comp a b) g]
        | [] => throwCatNF (CatNFError.normalizationError "whisker_right: empty segments")
      else if n == tensorObj then
        let fSegs ← aux f (depth + 1)
        let gSegs ← aux g (depth + 1)
        let fCol ← collapseSegList fSegs "tensorObj: empty left segments"
        let gCol ← collapseSegList gSegs "tensorObj: empty right segments"
        return [ExprSegment.tensor fCol gCol]
      else if n == leftUnitor then
        let fSegs ← aux f (depth + 1)
        match fSegs with
        | fSeg :: [] => return [ExprSegment.left_unitor fSeg]
        | a :: b :: _ => return [ExprSegment.left_unitor (ExprSegment.comp a b)]
        | [] => throwCatNF (CatNFError.normalizationError "left_unitor: empty segments")
      else if n == rightUnitor then
        let fSegs ← aux f (depth + 1)
        match fSegs with
        | fSeg :: [] => return [ExprSegment.right_unitor fSeg]
        | a :: b :: _ => return [ExprSegment.right_unitor (ExprSegment.comp a b)]
        | [] => throwCatNF (CatNFError.normalizationError "right_unitor: empty segments")
      else if n == braiding then
        let fSegs ← aux f (depth + 1)
        let gSegs ← aux g (depth + 1)
        let fCol ← collapseSegList fSegs "braiding: empty left segments"
        let gCol ← collapseSegList gSegs "braiding: empty right segments"
        return [ExprSegment.braid fCol gCol]
      else
        return [ExprSegment.raw e]
    | .app (.const n _) arg =>
      if n == categoryId then
        return [ExprSegment.id]
      else if n == isoHom then
        return [ExprSegment.iso_hom arg]
      else if n == isoInv then
        return [ExprSegment.iso_inv arg]
      else
        return [ExprSegment.raw e]
    | .app (.app (.app (.const n _) f) g) h =>
      if n == associator then
        let fSegs ← aux f (depth + 1)
        let gSegs ← aux g (depth + 1)
        let hSegs ← aux h (depth + 1)
        let fCol ← collapseSegList fSegs "associator: empty f segments"
        let gCol ← collapseSegList gSegs "associator: empty g segments"
        let hCol ← collapseSegList hSegs "associator: empty h segments"
        return [ExprSegment.associator fCol gCol hCol]
      else
        return [ExprSegment.raw e]
    | _ => return [ExprSegment.raw e]

  aux expr 0

def eraseIdentities (segments : List ExprSegment) : CatNFM (List ExprSegment) := do
  if segments.length > 1000 then
    throwCatNF (CatNFError.validationError "too many segments to process (max 1000)")
  let result := segments.filter (fun seg => match seg with | .id => false | _ => true)
  if result.isEmpty then
    throwCatNF (CatNFError.normalizationError "all segments were identities - cannot normalize empty expression")
  return result

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

def shuntIsomorphisms (segments : List ExprSegment) : MetaM (List ExprSegment) := do
  match ← (shuntIsomorphismsCat segments).run with
  | Except.ok r => return r
  | Except.error e => throwError (toString e)

def normalizeFunctorMaps (segments : List ExprSegment) : CatNFM (List ExprSegment) := do
  if segments.length > 1000 then
    throwCatNF (CatNFError.validationError "too many segments to process (max 1000)")
  return segments

def segmentToExpr (seg : ExprSegment) : CatNFM Expr := do
  validateExprSegment seg
  match seg with
  | .id => return mkCategoryId (mkConst (Name.mkSimple "_cat_nf_id_obj"))
  | .iso_hom iso => return mkIsoHom iso
  | .iso_inv iso => return mkIsoInv iso
  | .raw expr => return expr
  | .comp f g => do
    let fExpr ← segmentToExpr f
    let gExpr ← segmentToExpr g
    return mkCategoryComp fExpr gExpr
  | .functor_map F f => do
    let fExpr ← segmentToExpr f
    return mkFunctorMap F fExpr
  | .whisker_left F f => do
    let fExpr ← segmentToExpr f
    return mkWhiskerLeft F fExpr
  | .whisker_right f G => do
    let fExpr ← segmentToExpr f
    return mkWhiskerRight fExpr G
  | .tensor f g => do
    let fExpr ← segmentToExpr f
    let gExpr ← segmentToExpr g
    return mkTensorObj fExpr gExpr
  | .associator f g h => do
    let fExpr ← segmentToExpr f
    let gExpr ← segmentToExpr g
    let hExpr ← segmentToExpr h
    return mkApp3 (mkConst associator) fExpr gExpr hExpr
  | .left_unitor f => segmentToExpr f
  | .right_unitor f => segmentToExpr f
  | .braid f g => do
    let fExpr ← segmentToExpr f
    let gExpr ← segmentToExpr g
    return mkApp2 (mkConst braiding) fExpr gExpr

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
      return mkCategoryComp segExpr restExpr
  aux segments

/-- Kernel-only normalization: flatten then rebuild (no category rewrite steps). -/
def normalizeGoal (goal : Expr) (config : Config) : CatNFM (Expr × List AppliedRewrite) := do
  if goal.isMVar then
    throwCatNF (CatNFError.validationError "cannot normalize metavariable goals")
  validateConfig config
  let segments ← flattenComposition goal config
  let result ← rebuildExpression segments
  return (result, [])

def normalizeMonoidal (segments : List ExprSegment) (_config : Config) : CatNFM (List ExprSegment) := do
  validateExprSegments segments
  return segments

def flattenCompositionM (expr : Expr) (config : Config) : MetaM (List ExprSegment) := do
  match ← (flattenComposition expr config).run with
  | Except.ok segs => return segs
  | Except.error e => throwError (toString e)

def rebuildExpressionM (segments : List ExprSegment) : MetaM Expr := do
  match ← (rebuildExpression segments).run with
  | Except.ok e => return e
  | Except.error e => throwError (toString e)

/-- Structural normalization without category pipeline steps (see `CatNF.Category.Pipeline`). -/
def normalizeStructuralGoalM (goal : Expr) (config : Config) : MetaM (Expr × List AppliedRewrite) := do
  match ← (normalizeGoal goal config).run with
  | Except.ok r => return r
  | Except.error e => throwError (toString e)

end CatNF
