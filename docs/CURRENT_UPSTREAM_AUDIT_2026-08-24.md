# CatNF current-upstream audit — 2026-08-24

This document supersedes the June 2026 extraction queue **for current Mathlib contribution decisions**. The earlier ledger remains a historical record of normalization cases exercised on the 4.31 line.

## Audit baseline

- Lean: `leanprover/lean4:v4.34.0-rc2`
- Mathlib: `dc84fcbe9e049439c1c36d6db290cc0565f77788` (master, 2026-08-24)
- Current-baseline build status: **pending CI** until the audit branch workflow completes successfully.

The prior `v4.31.0-rc1` build record is historical and must not be represented as current compatibility evidence.

## Lessons from the first Mathlib attempt

Mathlib PR #40675 attempted to add four documentation examples for basic normalization patterns. It was closed unmerged shortly after creation.

The recorded PR metadata also showed a severe branch/base hygiene problem: despite the intended change being tiny, GitHub reported thousands of changed files and millions of deletions. Regardless of the mathematical content, that submission shape was not reviewable.

This creates two separate requirements for future work:

1. candidate value must be demonstrated independently of CatNF;
2. every upstream branch must be checked for a minimal diff before opening a PR.

## Current Mathlib has advanced

Several cases that the June ledger treated as possible upstream helpers are already present or better supported on current master.

A concrete example is `Functor.map_comp_assoc`, now explicitly present in `Mathlib/CategoryTheory/Functor/Basic.lean`. Mathlib documents that this theorem is supplied manually because the normal `reassoc` generation route is unavailable at that import point.

Current category-theory core files also enable category-specific `grind` support. This changes the relevant comparison baseline for normalization substantially.

Therefore the old normalization/example queue must not be replayed against current master without a full benchmark refresh.

## Strategic decision

CatNF is now primarily a **normalization benchmark and proof-friction laboratory**.

Its local tactic is valuable if it reveals recurring proof transformations that current Mathlib users repeatedly perform manually. The tactic itself is not the first upstream target.

The main diagnostic question is:

> Which normalization step performed by CatNF corresponds to a genuinely missing, reusable Mathlib theorem or attribute after current `simp`, `cat_disch`, `grind`, reassociated lemmas, and existing coherence tools are taken into account?

## Current benchmark matrix

Re-run representative cases across at least:

| Goal family | `simp` | `cat_disch` | `grind` | direct theorem/reassoc | CatNF |
|---|---:|---:|---:|---:|---:|
| identity cancellation | | | | | |
| associativity / reassociation | | | | | |
| functor map over identities/composition | | | | | |
| whiskering | | | | | |
| iso hom/inv cancellation | | | | | |
| naturality rearrangement | | | | | |
| monoidal coherence cases | | | | | |

For each failure or awkward proof, record the exact theorem sequence required by the best non-CatNF proof.

## Real-proof mining

Synthetic unit tests remain useful for determinism and regression, but upstream extraction requires real downstream evidence.

The current program should inspect a corpus of category-theory files and identify repeated proof fragments such as:

- explicit associativity rewrites around a standard theorem;
- repeated `simp only` bundles needed to normalize the same shape;
- recurring whiskering exchanges;
- iso cancellation patterns that are not handled by the intended simp normal form.

A candidate should ideally collapse multiple independent real proof sequences into one stable declaration.

## Decision table

| June candidate family | Current decision |
|---|---|
| basic identity/associativity examples | Retire as upstream plan |
| `Functor.map_comp_assoc`-style helper | Exists upstream; mark superseded |
| reverse `map_comp` aliases | Re-audit against current simp/rewrite conventions before considering |
| whiskering helper aliases | High duplication risk; real-proof evidence required |
| iso cancellation bundles | Benchmark current simp first |
| CatNF tactic | Keep local |
| monoidal normalization | Research; compare current coherence tooling before any extraction |

## Upstream branch hygiene gate

Before any future Mathlib PR is opened:

1. start from current Mathlib master;
2. inspect the exact three-dot diff against master;
3. verify only intended files are changed;
4. record changed-file count and declaration diff;
5. do not open the PR if unrelated repository history appears in the diff.

This is a mandatory gate after #40675.

## Acceptance gate for a CatNF-derived Mathlib PR

A candidate may move to `PR_READY` only when:

1. the current baseline builds;
2. current master/open PRs do not already contain the theorem or an equivalent mechanism;
3. current `simp`, `cat_disch`, `grind`, reassoc, and relevant coherence tools have been benchmarked;
4. at least three independent real proof sites demonstrate the same missing normalization fact, or one substantial development is clearly blocked by it;
5. the proposed theorem is more primitive/general than the local tactic implementation;
6. no CatNF tactic code is required by the submitted proof;
7. the upstream diff is demonstrably minimal and reviewable;
8. AI assistance is disclosed and the contributor independently understands every submitted declaration/proof.

## Immediate work queue

1. Obtain CI evidence on the current baseline.
2. Re-run the existing normalization corpus under current Mathlib automation.
3. Mark each old ledger case `EXISTS_UPSTREAM`, `SOLVED_BY_AUTOMATION`, `RESEARCH`, or `CANDIDATE`.
4. Mine real current category-theory proof files for recurring normalization boilerplate.
5. Only then extract the smallest surviving lemma/API improvement.
