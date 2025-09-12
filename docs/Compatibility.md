# Compatibility Table

This table shows the compatibility between `lean-cat-nf` versions and Lean 4/Mathlib4 versions.

## Lean 4 Compatibility

| lean-cat-nf | Lean 4 | Status |
|-------------|--------|--------|
| v1.0.0      | v4.8.0 | ✅ Supported |
| v1.0.0      | v4.7.0 | ✅ Supported |
| v1.0.0      | v4.6.0 | ❌ Not supported |
| v0.9.0      | v4.8.0 | ✅ Supported |
| v0.9.0      | v4.7.0 | ✅ Supported |
| v0.9.0      | v4.6.0 | ❌ Not supported |

## Mathlib4 Compatibility

| lean-cat-nf | Mathlib4 | Status |
|-------------|----------|--------|
| v1.0.0      | v4.8.0   | ✅ Supported |
| v1.0.0      | v4.7.0   | ✅ Supported |
| v1.0.0      | v4.6.0   | ❌ Not supported |
| v0.9.0      | v4.8.0   | ✅ Supported |
| v0.9.0      | v4.7.0   | ✅ Supported |
| v0.9.0      | v4.6.0   | ❌ Not supported |

## Versioning Policy

### Semantic Versioning (SemVer)

- **MAJOR**: Incremented for changes in tactic behavior
- **MINOR**: Incremented for the addition of new normalizers
- **PATCH**: Incremented for performance improvements and bug fixes

### Breaking Changes

- **v1.0.0**: Initial stable release
- **v0.9.0**: Pre-release with experimental features

### Migration Guide

#### From v0.9.0 to v1.0.0

No breaking changes. The API is stable.

#### From v0.8.0 to v0.9.0

- Added monoidal category support
- Changed default configuration options
- Updated performance targets

## Installation

### Latest Stable (v1.0.0)

```lean
require lean-cat-nf from git
  "https://github.com/fraware/lean-cat-nf.git" @ "v1.0.0"
```

### Latest Development

```lean
require lean-cat-nf from git
  "https://github.com/fraware/lean-cat-nf.git" @ "main"
```

### Specific Version

```lean
require lean-cat-nf from git
  "https://github.com/fraware/lean-cat-nf.git" @ "v0.9.0"
```

## Testing Compatibility

To test if your setup is compatible:

```lean
import Mathlib.Tactic.CatNF

-- Basic functionality test
example (f : X ⟶ Y) : f ≫ 𝟙 Y = f := by
  cat_nf

-- Monoidal functionality test
example (f : X ⟶ Y) (g : Y ⟶ Z) : (f ⊗ g) ⊗ h = f ⊗ (g ⊗ h) := by
  cat_nf
```

## Reporting Issues

If you encounter compatibility issues:

1. Check this table for supported versions
2. Try updating to the latest version
3. Create an issue with:
   - Your Lean 4 version
   - Your Mathlib4 version
   - Your lean-cat-nf version
   - A minimal reproduction case

## Future Compatibility

We plan to support:

- **Lean 4**: Latest and previous minor versions
- **Mathlib4**: Latest and previous minor versions
- **Backward compatibility**: At least 2 major versions

## Deprecation Policy

- **Deprecation warnings**: 6 months before removal
- **Breaking changes**: Only in major versions
- **Migration guides**: Provided for all breaking changes
