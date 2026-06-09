import Lean.Meta
import CatNF.Core.Normalize
import CatNF.Category.Basic
import CatNF.Category.FunctorWhisker
import CatNF.Category.IsoTransport
import CatNF.Monoidal.Core

open Lean Meta

namespace CatNF

/--
Narrow-scope normalization on flattened segments:
associativity/units, functor/whisker, isomorphism transport, then (when
`config.monoidal`) monoidal associator/unitor/braiding rewrites.
-/
def normalizeSegmentsM (segments : List ExprSegment) (config : Config) : MetaM (List ExprSegment) := do
  let afterAssoc ← normalizeAssocUnit segments
  let afterFunctor ← normalizeFunctorWhisker afterAssoc
  let afterIso ← normalizeIsoTransport afterFunctor
  if config.monoidal then
    CatNF.Monoidal.normalizeMonoidal afterIso
  else
    return afterIso

/--
Full normalization pipeline: flatten → category steps → rebuild.

This is the public entry point used by tactics, tests, and benchmarks.
Structural flatten/rebuild lives in `CatNF.Core.Normalize`; category rewrites
are orchestrated here so the kernel stays Mathlib-free.
-/
def normalizeGoalM (goal : Expr) (config : Config) : MetaM (Expr × List AppliedRewrite) := do
  let segments ← flattenCompositionM goal config
  let processed ← normalizeSegmentsM segments config
  let result ← rebuildExpressionM processed
  return (result, [])

end CatNF
