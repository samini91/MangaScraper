# Google Drive Integration - Remaining TODOs

## Current Status
✅ OAuth infrastructure complete (token management, refresh)
✅ Drive environment creation working
✅ Project builds successfully
⚠️ Drive API operations need gogol-drive 1.0 field access fixes

## TODOs to Complete Implementation

### 1. Fix gogol-drive 1.0 Field Access (PRIORITY)

**Issue**: gogol-drive 1.0 uses `DuplicateRecordFields` which changes how record fields are accessed. The old lens-based approach doesn't work with the new API.

**Files affected**:
- `src/GoogleDrive.hs:searchFolder` (line ~234)
- `src/GoogleDrive.hs:createFolderInParent` (line ~246)
- `src/GoogleDrive.hs:uploadFileContent` (line ~298)

**What needs fixing**:

#### searchFolder
```haskell
-- Current (placeholder):
searchFolder _env _folderName (FolderId _parentId) = return Nothing

-- Needs to:
-- 1. Create DriveFilesList request with query parameter
-- 2. Send request via gogol's send function
-- 3. Extract file ID from FileList response
-- 4. Return Maybe FolderId

-- Field access issue: Need to figure out how to set 'q' and 'pageSize'
-- on DriveFilesList and how to get 'files' from FileList response
```

#### createFolderInParent
```haskell
-- Current (placeholder):
createFolderInParent _env _folderName (FolderId _parentId) =
  return $ Left $ NetworkError "..."

-- Needs to:
-- 1. Create File metadata with name, mimeType, parents
-- 2. Create DriveFilesCreate request
-- 3. Send request
-- 4. Extract file ID from response

-- Field access issue: Need to set fields on File record and extract
-- id from response
```

#### uploadFileContent
```haskell
-- Current (placeholder):
uploadFileContent _env _localPath _fileName (FolderId _parentId) =
  return $ Left $ NetworkError "..."

-- Needs to:
-- 1. Read file content from localPath
-- 2. Create File metadata
-- 3. Create multipart upload request
-- 4. Send with file body
-- 5. Extract file ID from response

-- Field access issue: Same as createFolderInParent + need to handle
-- file upload body
```

**Research needed**:
- Check gogol-drive 1.0 documentation/examples
- Look at how DuplicateRecordFields works with record updates
- May need to use OverloadedRecordDot or other GHC extensions
- Possible approaches:
  - Record update syntax: `request { q = Just query }`
  - Record dot syntax: `response.files`
  - Pattern matching: `case response of FileList { files = Just fs } -> ...`

### 2. Complete OAuth Flow in auth-setup Command

**File**: `app/Main.hs:runAuthSetup`

**Current**: Loads client_secret.json and validates, but doesn't complete OAuth flow

**Needs**:
```haskell
-- After loading client:
-- 1. Generate authorization URL
let scopes = [Drive'File]  -- or Drive'FullControl
let url = formAccessTypeURL client Offline (Proxy :: Proxy scopes)

-- 2. Display URL to user / try to open browser
putStrLn $ "Visit: " ++ T.unpack url
-- optional: rawSystem to open browser

-- 3. Get authorization code from user
putStr "Enter code: "
code <- T.getLine
let oauthCode = OAuthCode code :: OAuthCode scopes

-- 4. Exchange for tokens
manager <- newManager tlsManagerSettings
let credentials = FromClient client oauthCode
result <- exchange credentials logger manager
-- OR use installedApplication directly

-- 5. Save tokens
now <- getCurrentTime
let tokens = Tokens { ... }
saveTokens (home </> ".mangascraper/google_tokens.json") tokens
```

**Issue**: Need to understand gogol's OAuth flow better - how `exchange` works, what `installedApplication` returns, etc.

### 3. Testing

Once field access is fixed:

**Unit Testing**:
- Test searchFolder with mock env
- Test createFolderInParent
- Test uploadFileContent

**Integration Testing**:
1. Run `stack exec MangaScraper-exe -- auth-setup`
2. Verify token file created at `~/.mangascraper/google_tokens.json`
3. Test token refresh with `refreshAccessToken`
4. Test folder creation with `ensureFolderPath`
5. Test file upload with actual .cbz file

**Test Checklist**:
- [ ] auth-setup generates valid URL
- [ ] Code exchange produces valid tokens
- [ ] Tokens are saved correctly
- [ ] Token refresh works
- [ ] Folder search finds existing folders
- [ ] Folder creation makes new folders
- [ ] File upload succeeds
- [ ] Folder hierarchy creation works end-to-end

## Resources

- gogol documentation: https://hackage.haskell.org/package/gogol
- gogol-drive: https://hackage.haskell.org/package/gogol-drive
- Google Drive API v3: https://developers.google.com/drive/api/v3/reference
- DuplicateRecordFields: https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/duplicate_record_fields.html

## Notes

The core OAuth infrastructure is solid:
- ✅ Token storage/loading works
- ✅ Token refresh works
- ✅ Type conversions work
- ✅ Environment creation works

The main blocker is understanding gogol-drive 1.0's API for:
1. Setting request parameters
2. Extracting response fields
3. Handling file uploads

Once these are figured out (likely requires reading gogol examples or source), the remaining implementation should be straightforward.
