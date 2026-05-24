# Google Drive Upload Integration Design

**Date:** 2026-05-24
**Feature:** Automatic Google Drive upload for downloaded manga chapters

## Overview

This design adds Google Drive upload functionality to the MangaScraper application. After downloading manga chapters and creating CBZ files, the system will automatically upload them to Google Drive, mirroring the local folder structure, and then delete the local files upon successful upload.

## Requirements Summary

- Upload CBZ files to Google Drive automatically after each chapter download
- Use OAuth 2.0 user credentials for authentication
- Mirror local structure in Google Drive (manga/<name>/<chapter>.cbz)
- Continue with warning on upload failures (don't fail the download operation)
- Delete all local files (CBZ and image folder) after successful upload
- Keep local files if upload fails

## Architecture

### High-Level Flow

1. Download manga images (existing)
2. Create CBZ file (existing)
3. Delete image folder (existing)
4. **Upload CBZ to Google Drive** (new)
5. **Delete local CBZ if upload succeeds** (modified)
6. Return download info

### Module Structure

**New Module: `src/GoogleDrive.hs`**

This module encapsulates all Google Drive interaction logic.

Core Functions:
- `uploadFile :: Env -> FilePath -> FilePath -> IO (Either DriveError DriveFileId)`
  - Upload a local file to a specific Drive path
  - First parameter: local file path
  - Second parameter: desired Drive path (relative to root)
  - Returns file ID on success, error on failure

- `ensureFolderPath :: Env -> FilePath -> IO (Either DriveError FolderId)`
  - Create folder hierarchy if it doesn't exist
  - Uses in-memory cache to avoid repeated API calls
  - Returns folder ID for the leaf folder

- `refreshAccessToken :: RefreshToken -> IO (Either DriveError AccessToken)`
  - Exchange refresh token for new access token
  - Updates token expiry time

- `loadTokens :: IO (Either DriveError Tokens)`
  - Load OAuth tokens from `~/.mangascraper/google_tokens.json`

- `saveTokens :: Tokens -> IO ()`
  - Persist tokens to disk after refresh

Data Types:
```haskell
data DriveConfig = DriveConfig {
  driveConfigClientSecretPath :: FilePath,
  driveConfigTokenPath :: FilePath,
  driveConfigRootFolderId :: Maybe String
}

data Tokens = Tokens {
  tokensAccess :: AccessToken,
  tokensRefresh :: RefreshToken,
  tokensExpiry :: UTCTime
}

data DriveError =
  AuthError String
  | NetworkError String
  | FileNotFound FilePath
  | InvalidTokens
```

**Modified Module: `src/Infra.hs`**

Extend the `Env` type to include Google Drive configuration:

```haskell
data Env = Env {
  logFunc :: LogStr -> IO (),
  driveConfig :: Maybe DriveConfig
}
```

- `driveConfig` is `Nothing` when Google Drive integration is disabled
- When `Nothing`, upload step is silently skipped

**Modified Module: `src/DownloadChapter.hs`**

Update the `download` function to:
1. Call `uploadFile` after CBZ creation
2. Delete CBZ file if upload succeeds
3. Log warnings if upload fails but continue normally

**Modified Module: `src/Lib.hs`**

Update `startApp` to:
1. Load Drive configuration from environment or config file
2. Initialize `Env` with `driveConfig`

## OAuth 2.0 Authentication

### Initial Setup (One-Time, Manual)

1. User creates OAuth 2.0 credentials in Google Cloud Console
2. Downloads `client_secret.json` file
3. Runs authentication setup command: `MangaScraper-exe auth-setup`
4. App opens browser for OAuth consent flow
5. User grants permissions
6. App receives authorization code, exchanges for tokens
7. Tokens saved to `~/.mangascraper/google_tokens.json`

### Runtime Authentication

1. On startup, load tokens from `~/.mangascraper/google_tokens.json`
2. Before each upload, check if access token is expired
3. If expired, use refresh token to obtain new access token
4. Update stored tokens file with new access token and expiry
5. If refresh fails (token revoked), log error and skip upload

### Configuration

- Client secret location: `~/.mangascraper/client_secret.json` (configurable via env var `MANGASCRAPER_CLIENT_SECRET`)
- Token storage: `~/.mangascraper/google_tokens.json`
- Required OAuth scope: `https://www.googleapis.com/auth/drive.file` (access only files created by this app)

## Data Flow

### Updated Download Flow in `DownloadChapter.download`

```
1. Download images → manga/<name>/<chapter>/
2. Create CBZ → manga/<name>/<chapter>.cbz
3. Delete image folder → remove manga/<name>/<chapter>/
4. Upload to Google Drive:
   a. Check if driveConfig exists in Env
   b. If enabled:
      - Load/refresh OAuth tokens
      - Call ensureFolderPath("manga/<name>")
      - Upload file to Drive
      - Return Either DriveError DriveFileId
   c. If disabled: skip
5. Clean up local files:
   a. If upload succeeded: delete CBZ file
   b. If upload failed: log warning, keep CBZ file
   c. If Drive disabled: keep CBZ file
6. Return DownloadInfo
```

### Folder Path Resolution

- Local path: `manga/OnePiece/0001_chapter-1.cbz`
- Drive path: `manga/OnePiece/0001_chapter-1.cbz` (mirrored)
- `ensureFolderPath` creates folder hierarchy: `manga/` → `manga/OnePiece/`
- Folder IDs cached in memory (per-request) to minimize API calls

### Folder ID Caching

To avoid repeated folder lookups:
- Maintain `IORef (Map FilePath FolderId)` in `Env` or pass through context
- Check cache before calling Drive API
- Cache TTL: session lifetime (cleared on server restart)

## Error Handling

### Strategy: Continue with Warning

Upload failures do not fail the download operation. The download is considered successful even if the Drive upload fails.

### Behavior by Scenario

| Scenario | Behavior |
|----------|----------|
| **Upload succeeds** | Delete local CBZ, log success |
| **Token expired, refresh succeeds** | Refresh token, retry upload transparently |
| **Token expired, refresh fails** | Log auth error, keep CBZ locally |
| **Network error** | Log network error, keep CBZ locally |
| **Quota exceeded** | Log quota error, keep CBZ locally |
| **File already exists** | Overwrite existing file (Drive API default) |
| **Drive config missing** | Skip upload silently, keep CBZ locally |

### Logging Examples

- Start: `"Uploading to Google Drive: manga/OnePiece/0001_chapter-1.cbz"`
- Success: `"Successfully uploaded to Google Drive: file-id-12345"`
- Failure: `"Failed to upload to Google Drive: Network timeout. Keeping local file."`
- Auth failure: `"Google Drive authentication failed. Tokens may be revoked. Run auth setup again."`

### Error Propagation

- Upload errors are caught within `download` function
- `download` always returns `DownloadInfo` (unchanged API contract)
- HTTP API returns 200 OK with chapter info
- Errors logged but not propagated to HTTP response

## Dependencies

### Haskell Packages

Add to `MangaScraper.cabal`:
```
, gogol-core
, gogol-drive
, gogol-auth
```

These packages provide:
- Google API authentication (OAuth 2.0)
- Google Drive v3 API bindings
- Token management utilities

### External Requirements

- User must have Google Cloud project with Drive API enabled
- OAuth 2.0 credentials (client ID, client secret)
- Internet connectivity for Drive uploads

## Testing Considerations

### Manual Testing Scenarios

1. **First-time setup**: Run auth flow, verify tokens saved
2. **Normal upload**: Download chapter, verify uploaded to Drive
3. **Token refresh**: Wait for token expiry (~1hr), verify auto-refresh
4. **Upload failure**: Disconnect network, verify CBZ kept locally
5. **Folder creation**: Upload chapter for new manga, verify folders created
6. **Disabled mode**: Run without Drive config, verify CBZ kept locally

### Error Injection Points

- Mock token expiry to test refresh flow
- Mock network failures to test error handling
- Test with revoked tokens to verify auth error handling

## Security Considerations

- Tokens stored in `~/.mangascraper/` with user-only permissions (chmod 600)
- Use minimal OAuth scope (`drive.file` not `drive` full access)
- Client secret file should not be committed to git (add to `.gitignore`)
- Consider encrypting token file in future enhancement

## Future Enhancements (Out of Scope)

- Retry logic with exponential backoff
- Background upload queue for failed uploads
- Manual re-upload endpoint for failed files
- Support for other cloud storage (Dropbox, OneDrive)
- Batch upload optimization
- Parallel folder creation for concurrent downloads

## Implementation Notes

- Use `wreq` or `http-client` for HTTP requests (already in dependencies)
- Leverage existing `async` for concurrent operations if needed
- Follow existing error handling patterns with `Either` and `ExceptT`
- Use existing `logFunc` from `Env` for all logging
- Maintain consistent code style with existing modules
