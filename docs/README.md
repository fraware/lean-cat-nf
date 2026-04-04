# CatNF documentation

This folder holds conceptual and compatibility notes for **CatNF** (category normal form for Lean 4). For install, Lake setup, API signatures, and CI commands, start with the [project README](../README.md). For build workflows and review expectations, see [CONTRIBUTING](../CONTRIBUTING.md).

## Contents

| Document | Purpose |
|----------|---------|
| [UserGuide.md](UserGuide.md) | Choosing an entry point, config limits, metavariables, `raw` segments, troubleshooting, and where to copy examples from tests. |
| [WhatCountsAsCanonical.md](WhatCountsAsCanonical.md) | Intended normal forms (composition, functors, monoidal structure) and how they relate to the current implementation. |
| [Compatibility.md](Compatibility.md) | Lean / Mathlib pins, depending on this package from Lake, and bump workflow. |

## Quick links

- **Primary API**: `CatNF.Core.normalizeGoal` / `normalizeGoalM` — see the README “Usage” and “API reference” sections.
- **Practical usage**: [UserGuide.md](UserGuide.md) — when to use which function and what errors mean.
- **Tests**: `src/CatNF/Tests/` — concrete expressions and expectations for behavior that is implemented today.
