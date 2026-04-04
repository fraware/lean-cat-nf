## What changed

## Why

## How to verify

- `lake build`
- `lake exe test-runner` (on Windows see CONTRIBUTING.md if linking fails)
- (optional) `lake exe bench`
- (optional) Docker build and `docker run ... --help` when container behavior changes

## Checklist

- [ ] Lean and Mathlib pins match `lean-toolchain` and `Lakefile.lean`
- [ ] No new `sorry` or `admit` in library or benchmark sources where policy forbids them
- [ ] README or `docs/` updated if user-facing behavior or install paths changed
