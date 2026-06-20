# Google Drive Integration - Build Compatibility Issue

## Problem Summary

The Google Drive implementation cannot build due to fundamental incompatibilities between gogol library versions and the modern Haskell ecosystem.

## Root Cause

**Gogol 1.0.0** (released 2022):
- ✅ Compatible with modern GHC (9.10.3) and dependencies
- ❌ **Dropped all field lenses** - uses `NoFieldSelectors` extension
- ❌ No lens-based API for setting/getting record fields
- ❌ Requires `OverloadedRecordDot` or record update syntax

**Gogol 0.5.0** (released ~2018):
- ✅ Has full lens-based API (`flQ`, `flFiles`, `fId`, etc.)
- ❌ **Incompatible with aeson 2.x** (modern stack uses aeson 2.2.5.0)
- ❌ Expects `HashMap Text Value` but aeson 2.x uses `Data.Aeson.KeyMap.KeyMap Value`
- ❌ Compilation error in `gogol-core-0.5.0/src/Network/Google/Data/JSON.hs:67`

## Attempted Solutions

### 1. Use Gogol 1.0.0 with OverloadedRecordDot ❌

**Tried**: Enabling `OverloadedRecordDot` extension to access fields without lenses

**Result**: Fields not exported due to `NoFieldSelectors` - can't use record syntax

**Error**: `Not in scope: record field 'name'`

### 2. Downgrade to Gogol 0.5.0 ❌

**Tried**: Using gogol-0.5.0 with `allow-newer` to ignore version bounds

**Result**: Type mismatch in aeson Object handling

**Error**:
```
Couldn't match type: Data.Aeson.KeyMap.KeyMap Value
                 with: HashMap Text Value
```

### 3. Add Compatibility Shims ❌

Would require patching gogol-core source to handle both aeson versions - not feasible without forking.

## Possible Solutions

### Option 1: Use Older LTS Resolver (Downgrade Everything)

**Action**: Switch to an LTS that includes gogol 0.5.0 natively
- Likely requires LTS 12-14 (GHC 8.4-8.6)
- Would break compatibility with other dependencies
- Not recommended for new development

### Option 2: Fork and Patch Gogol 0.5.0

**Action**: Create a patched version of gogol-core 0.5.0
- Update aeson compatibility layer
- Change `HashMap Text Value` to `KeyMap Value`
- Maintain fork indefinitely
- Significant effort

### Option 3: Write OverloadedLabels/Generic-Lens Wrapper

**Action**: Create generic lens accessors for gogol 1.0 types
- Use `generic-lens` or `generic-optics`
- Write wrappers to expose field access
- May be complex due to `NoFieldSelectors`

### Option 4: Use REST API Directly

**Action**: Abandon gogol entirely, use http-client with aeson
- Write own Drive API bindings
- Full control over implementation
- More manual work but guaranteed compatibility
- Example libraries: `req`, `wreq`

### Option 5: Accept gogol 1.0 Limitations

**Action**: Rewrite code to work without lenses
- Use record pattern matching: `File { name = Just n, .. }`
- Use record updates: `file { name = Just "new" }`
- May require significant code restructuring
- Investigate if `HasField` instances are available

## Current Code State

The implementation branch has:
- ✅ Complete OAuth flow logic
- ✅ Token refresh mechanism
- ✅ Folder creation logic
- ✅ File upload logic
- ❌ Uses lens-based API that doesn't compile

**Code location**: `drive` branch, commit `beb95a3`

## Recommendations

### Short Term
1. **Investigate Option 5** - Check if gogol-drive 1.0 exports `HasField` instances
   ```bash
   stack exec -- ghci
   > :browse Gogol.Drive.Types
   ```
2. If `HasField` exists, rewrite using `getField @"name"` pattern

### Medium Term
1. **Option 4** - Consider direct REST API implementation
   - More maintainable long-term
   - No dependency on gogol updates
   - Full type safety with custom types

### Long Term
1. Monitor gogol project for lens support or better record access
2. Consider contributing to gogol if actively maintained

## Technical Details

### Aeson Version Change

**aeson < 2.0**:
```haskell
type Object = HashMap Text Value
```

**aeson >= 2.0**:
```haskell
type Object = KeyMap Value  -- KeyMap from aeson package
```

This change breaks all code that expects `HashMap Text Value`.

### NoFieldSelectors Impact

**With field selectors** (gogol 0.5):
```haskell
newFile & fName .~ Just "folder"  -- Works
```

**Without field selectors** (gogol 1.0):
```haskell
newFile & fName .~ Just "folder"  -- Error: fName not in scope
file { name = Just "folder" }      -- Error: name not exported
```

### Gogol 1.0 Alternative Patterns

If `HasField` instances exist:
```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}

import GHC.Records (getField, setField)

-- Reading
let name = getField @"name" fileRecord

-- Writing (with generic-lens)
import Data.Generics.Product.Fields (field)
file & field @"name" .~ Just "folder"
```

## Resources

- [Gogol 1.0 Release Discussion](https://discourse.haskell.org/t/gogol-1-0-0-drops-field-lenses/5569)
- [Gogol GitHub](https://github.com/brendanhay/gogol)
- [Google Drive REST API v3](https://developers.google.com/drive/api/v3/reference)
- [generic-lens Documentation](https://hackage.haskell.org/package/generic-lens)

## Next Steps

**User Decision Required**:

Which approach should we pursue?
1. Investigate gogol 1.0 with generic-lens
2. Implement direct REST API calls
3. Use an older LTS resolver
4. Defer Google Drive integration

The implementation is complete conceptually, but needs a viable build path.
