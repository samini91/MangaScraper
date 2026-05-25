# Google Drive OAuth and API Implementation Design

**Date:** 2026-05-25
**Feature:** Complete OAuth flow and Drive API integration using gogol library

## Overview

This design completes the Google Drive integration by implementing the OAuth authentication flow and real Drive API operations. The existing skeleton in `GoogleDrive.hs` has data types and stubs - this implementation replaces the stubs with working OAuth and API calls using the gogol library.

## Scope

**In Scope:**
- Initial OAuth authorization flow (auth-setup command)
- Token refresh mechanism
- Folder creation with search/reuse logic
- File upload with multipart encoding
- Error handling and logging
- Type conversions between custom types and gogol types

**Out of Scope:**
- Retry logic with exponential backoff (future enhancement)
- Background upload queue (future enhancement)
- Manual re-upload endpoint (future enhancement)

## Architecture Overview

### Integration Strategy

The implementation bridges the existing `GoogleDrive` module interface with gogol's type system:

1. **Keep existing public API** - `uploadFile`, `ensureFolderPath`, `loadTokens`, etc. remain unchanged
2. **Use gogol internally** - OAuth and Drive operations use gogol types and functions
3. **Convert between types** - Translate between custom `Tokens`/`DriveConfig` and gogol's `AuthorizedUser`/`OAuthClient`

### Module Structure

```
GoogleDrive module
├── Public API (unchanged)
│   ├── uploadFile :: DriveConfig -> AccessToken -> FilePath -> FilePath -> IO (Either DriveError DriveFileId)
│   ├── ensureFolderPath :: DriveConfig -> AccessToken -> FilePath -> IO (Either DriveError FolderId)
│   ├── loadTokens :: FilePath -> IO (Either DriveError Tokens)
│   ├── saveTokens :: FilePath -> Tokens -> IO ()
│   ├── refreshAccessToken :: FilePath -> RefreshToken -> IO (Either DriveError AccessToken)
│   └── getValidToken :: DriveConfig -> IO (Either DriveError AccessToken)
│
├── OAuth Management (new implementations)
│   ├── performInitialAuth :: OAuthClient -> IO (Either DriveError AuthorizedUser)
│   ├── loadOAuthClient :: FilePath -> IO (Either DriveError OAuthClient)
│   ├── authorizedUserFromTokens :: Tokens -> OAuthClient -> AuthorizedUser
│   └── tokensFromAuthorizedUser :: AuthorizedUser -> UTCTime -> Tokens
│
├── Drive API Operations (new implementations)
│   ├── searchFolder :: Env -> Text -> FolderId -> IO (Maybe FolderId)
│   ├── createFolder :: Env -> Text -> FolderId -> IO (Either DriveError FolderId)
│   └── uploadFileContent :: Env -> FilePath -> Text -> FolderId -> IO (Either DriveError DriveFileId)
│
└── Helper Functions (new)
    ├── newDriveEnv :: AuthorizedUser -> Logger -> Manager -> IO Env
    └── catchDriveErrors :: IO a -> IO (Either DriveError a)
```

### Dependencies

**Already in project:**
- `http-client`, `http-client-tls` - for Manager
- `gogol`, `gogol-core`, `gogol-drive` - just added in previous session
- `aeson`, `text`, `bytestring` - existing

**New imports needed:**
```haskell
import Gogol (Env, newEnv, send, upload, runResourceT)
import Gogol.Auth (Credentials(..))
import Gogol.Auth.InstalledApplication
import Gogol.Auth.ServiceAccount (authorizedUserToken)
import Gogol.Drive
import Gogol.Drive.Files.Create
import Gogol.Drive.Files.List
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
```

## OAuth Flow Implementation

### Client Secret Format

The user downloads `client_secret.json` from Google Cloud Console:

```json
{
  "installed": {
    "client_id": "123456.apps.googleusercontent.com",
    "client_secret": "GOCSPX-xxxxxxxxxxxx",
    "redirect_uris": ["urn:ietf:wg:oauth:2.0:oob"],
    "auth_uri": "https://accounts.google.com/o/oauth2/auth",
    "token_uri": "https://oauth2.googleapis.com/token"
  }
}
```

### Loading OAuth Client

```haskell
loadOAuthClient :: FilePath -> IO (Either DriveError OAuthClient)
loadOAuthClient path = do
  exists <- doesFileExist path
  if not exists
    then return $ Left $ AuthError "Client secret file not found"
    else do
      content <- BL.readFile path
      case eitherDecode content of
        Left err -> return $ Left $ AuthError ("Invalid client_secret.json: " ++ err)
        Right json -> do
          -- Extract installed.client_id and installed.client_secret
          let clientId = json ^? key "installed" . key "client_id" . _String
          let clientSecret = json ^? key "installed" . key "client_secret" . _String
          case (clientId, clientSecret) of
            (Just cid, Just csec) ->
              return $ Right $ OAuthClient (ClientId cid) (ClientSecret csec)
            _ -> return $ Left $ AuthError "Missing client_id or client_secret"
```

### Initial Authorization (auth-setup command)

New executable command handler in `app/Main.hs`:

```haskell
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["auth-setup"] -> runAuthSetup
    _ -> startApp  -- existing behavior

runAuthSetup :: IO ()
runAuthSetup = do
  putStrLn "=== Google Drive Authentication Setup ==="

  -- 1. Load client secret
  home <- getHomeDirectory
  let secretPath = home </> ".mangascraper/client_secret.json"
  clientResult <- loadOAuthClient secretPath

  case clientResult of
    Left err -> do
      putStrLn $ "ERROR: " ++ show err
      putStrLn "\nPlease ensure client_secret.json exists at:"
      putStrLn $ "  " ++ secretPath
      exitFailure

    Right client -> do
      -- 2. Generate authorization URL
      let url = formAccessTypeURL client Offline (Proxy :: Proxy '[Drive'File])

      putStrLn "\nOpening browser for authorization..."
      putStrLn $ "URL: " ++ T.unpack url

      -- Try to open browser (optional, user can manually copy URL)
      _ <- tryOpenBrowser url

      -- 3. Get authorization code from user
      putStrLn "\nPlease authorize the application and paste the code here:"
      putStr "Authorization code: "
      hFlush stdout
      codeText <- T.strip . T.pack <$> getLine

      -- 4. Exchange code for tokens
      let oauthCode = OAuthCode codeText :: OAuthCode '[Drive'File]
      let credentials = installedApplication client oauthCode

      -- 5. Get initial token
      manager <- newManager tlsManagerSettings
      logger <- newStdoutLogger Info

      tokenResult <- try $ authorizedUserToken credentials Nothing logger manager

      case tokenResult of
        Left (err :: SomeException) -> do
          putStrLn $ "\nERROR: Failed to exchange code for tokens"
          putStrLn $ "  " ++ displayException err
          exitFailure

        Right oauthToken -> do
          -- 6. Extract and save tokens
          now <- getCurrentTime
          let expiry = _tokenExpiry oauthToken
          let tokens = Tokens
                { tokensAccess = AccessToken (_tokenAccess oauthToken)
                , tokensRefresh = RefreshToken (fromMaybe "" $ _tokenRefresh oauthToken)
                , tokensExpiry = expiry
                }

          let tokenPath = home </> ".mangascraper/google_tokens.json"
          saveTokens tokenPath tokens

          putStrLn "\n✓ Authentication successful!"
          putStrLn $ "Tokens saved to: " ++ tokenPath
          putStrLn "\nYou can now use MangaScraper with Google Drive integration."
```

**Helper function:**

```haskell
tryOpenBrowser :: Text -> IO ()
tryOpenBrowser url = do
  os <- getOS
  void $ case os of
    "darwin" -> rawSystem "open" [T.unpack url]
    "linux"  -> rawSystem "xdg-open" [T.unpack url]
    _        -> return ExitSuccess  -- Windows or unknown, user copies manually
  where
    getOS = return System.Info.os
```

### Token Refresh Implementation

Replace the stub at line 116:

```haskell
refreshAccessToken :: FilePath -> RefreshToken -> IO (Either DriveError AccessToken)
refreshAccessToken clientSecretPath (RefreshToken refreshToken) = do
  -- 1. Load OAuth client
  clientResult <- loadOAuthClient clientSecretPath

  case clientResult of
    Left err -> return $ Left err
    Right client -> do
      -- 2. Create AuthorizedUser from stored refresh token
      let authUser = AuthorizedUser
            { _userId = _clientId client
            , _userSecret = _clientSecret client
            , _userRefresh = RefreshToken refreshToken
            }

      -- 3. Request new access token
      manager <- newManager tlsManagerSettings
      logger <- newStdoutLogger Error  -- Only log errors during refresh

      tokenResult <- try $ authorizedUserToken authUser Nothing logger manager

      case tokenResult of
        Left (err :: ServiceError) ->
          case err ^. serviceCode of
            401 -> return $ Left InvalidTokens  -- Refresh token expired/revoked
            _   -> return $ Left $ AuthError ("Token refresh failed: " ++ show err)

        Left (err :: SomeException) ->
          return $ Left $ NetworkError ("Network error during refresh: " ++ displayException err)

        Right oauthToken -> do
          let newAccessToken = AccessToken (_tokenAccess oauthToken)
          return $ Right newAccessToken
```

### Type Conversions

```haskell
-- Convert custom Tokens to gogol's AuthorizedUser
authorizedUserFromTokens :: Tokens -> OAuthClient -> AuthorizedUser
authorizedUserFromTokens (Tokens _ (RefreshToken refresh) _) client =
  AuthorizedUser
    { _userId = _clientId client
    , _userSecret = _clientSecret client
    , _userRefresh = RefreshToken refresh
    }

-- Convert gogol's OAuthToken to custom Tokens
tokensFromOAuthToken :: OAuthToken s -> UTCTime -> Tokens
tokensFromOAuthToken oauthToken expiry =
  Tokens
    { tokensAccess = AccessToken (_tokenAccess oauthToken)
    , tokensRefresh = RefreshToken (fromMaybe "" $ _tokenRefresh oauthToken)
    , tokensExpiry = expiry
    }
```

### Required OAuth Scopes

Use gogol's built-in scope type: `Drive'File`

This scope grants access only to files created by this application (most secure).

**Note:** In gogol 1.0, scopes are type-level strings. The scope `Drive'File` corresponds to `"https://www.googleapis.com/auth/drive.file"`.

## Drive API Operations

### Creating Drive Environment

```haskell
newDriveEnv :: AuthorizedUser -> (LogStr -> IO ()) -> Manager -> IO Env
newDriveEnv authUser logFunc manager = do
  let credentials = FromClient (_oauthClient authUser) (_refreshToken authUser)

  env <- newEnv credentials
    & envManager .~ manager
    & envLogger .~ customLogger logFunc

  return env

  where
    customLogger :: (LogStr -> IO ()) -> Logger
    customLogger lf level msg =
      lf $ toLogStr $ "[" ++ show level ++ "] " ++ msg
```

### Folder Search and Creation

Replace stub at line 162:

```haskell
-- Search for existing folder
searchFolder :: Env -> Text -> FolderId -> IO (Maybe FolderId)
searchFolder env folderName (FolderId parentId) = do
  let query = T.concat
        [ "name = '", folderName, "'"
        , " and mimeType = 'application/vnd.google-apps.folder'"
        , " and '", T.pack parentId, "' in parents"
        , " and trashed = false"
        ]

  response <- runResourceT $ send env
    (DriveFilesList & flQ (Just query) & flPageSize (Just 1))

  case response ^. flrFiles of
    Just (file:_) -> return $ FolderId <$> (file ^. fId)
    _ -> return Nothing

-- Create new folder
createFolder :: Env -> Text -> FolderId -> IO (Either DriveError FolderId)
createFolder env folderName (FolderId parentId) = catchDriveErrors $ do
  let metadata = file'
        & fName (Just folderName)
        & fMimeType (Just "application/vnd.google-apps.folder")
        & fParents (Just [T.pack parentId])

  created <- runResourceT $ send env (DriveFilesCreate metadata Nothing)

  case created ^. fId of
    Just fileId -> return $ FolderId (T.unpack fileId)
    Nothing -> throwM $ NetworkError "Failed to get folder ID from response"

-- Full hierarchy creation (replaces stub)
createFolderHierarchy :: DriveConfig -> AccessToken -> FolderId -> [FilePath] -> IO (Either DriveError FolderId)
createFolderHierarchy _ _ parentId [] = return $ Right parentId
createFolderHierarchy config (AccessToken _) parentId (folderName:rest) = do
  -- 1. Load credentials and create env
  client <- loadOAuthClient (driveConfigClientSecretPath config)
  case client of
    Left err -> return $ Left err
    Right oauthClient -> do
      tokens <- loadTokens (driveConfigTokenPath config)
      case tokens of
        Left err -> return $ Left err
        Right toks -> do
          let authUser = authorizedUserFromTokens toks oauthClient
          manager <- newManager tlsManagerSettings
          logger <- newStdoutLogger Error
          env <- newDriveEnv authUser logger manager

          -- 2. Search for existing folder
          existing <- searchFolder env (T.pack folderName) parentId

          -- 3. Use existing or create new
          folderIdResult <- case existing of
            Just fid -> return $ Right fid
            Nothing -> createFolder env (T.pack folderName) parentId

          -- 4. Recurse for remaining path components
          case folderIdResult of
            Left err -> return $ Left err
            Right folderId ->
              if null rest
                then return $ Right folderId
                else createFolderHierarchy config (AccessToken "") folderId rest
```

### File Upload

Replace stub at line 195:

```haskell
uploadFileContent :: Env -> FilePath -> Text -> FolderId -> IO (Either DriveError DriveFileId)
uploadFileContent env localPath fileName (FolderId parentId) = catchDriveErrors $ do
  -- 1. Read file content
  fileContent <- BS.readFile localPath

  -- 2. Create metadata
  let metadata = file'
        & fName (Just fileName)
        & fParents (Just [T.pack parentId])
        & fMimeType (Just "application/x-cbz")

  -- 3. Upload with multipart
  let body = sourceBody fileContent

  uploaded <- runResourceT $ upload env
    (DriveFilesCreate metadata (Just body))

  -- 4. Extract file ID
  case uploaded ^. fId of
    Just fileId -> return $ DriveFileId (T.unpack fileId)
    Nothing -> throwM $ NetworkError "Failed to get file ID from upload response"

-- Update main uploadFile function
uploadFile :: DriveConfig -> AccessToken -> FilePath -> FilePath -> IO (Either DriveError DriveFileId)
uploadFile config accessToken localPath drivePath = do
  -- Check if local file exists
  exists <- doesFileExist localPath
  if not exists
    then return $ Left $ FileNotFound localPath
    else do
      -- Setup
      clientResult <- loadOAuthClient (driveConfigClientSecretPath config)
      case clientResult of
        Left err -> return $ Left err
        Right client -> do
          tokensResult <- loadTokens (driveConfigTokenPath config)
          case tokensResult of
            Left err -> return $ Left err
            Right tokens -> do
              let authUser = authorizedUserFromTokens tokens client
              manager <- newManager tlsManagerSettings
              logger <- newStdoutLogger Error
              env <- newDriveEnv authUser logger manager

              -- Get folder path
              let driveFolder = takeDirectory drivePath
              let pathParts = splitDirectories drivePath
              let fileName = if null pathParts then "" else last pathParts

              -- Ensure folder exists
              folderResult <- ensureFolderPath config accessToken driveFolder
              case folderResult of
                Left err -> return $ Left err
                Right folderId ->
                  uploadFileContent env localPath (T.pack fileName) folderId
```

## Error Handling

### Catching Gogol Exceptions

```haskell
catchDriveErrors :: IO a -> IO (Either DriveError a)
catchDriveErrors action =
  (Right <$> action)
    `catch` handleServiceError
    `catch` handleHttpException
    `catch` handleGenericException
  where
    handleServiceError :: ServiceError -> IO (Either DriveError a)
    handleServiceError err =
      case err ^. serviceCode of
        401 -> return $ Left $ AuthError "Unauthorized - tokens expired or invalid"
        403 -> return $ Left $ AuthError "Forbidden - check OAuth scopes or permissions"
        404 -> return $ Left $ NetworkError "Resource not found in Google Drive"
        429 -> return $ Left $ NetworkError "Rate limit exceeded - please try again later"
        _   -> return $ Left $ NetworkError ("Drive API error: " ++ show err)

    handleHttpException :: HttpException -> IO (Either DriveError a)
    handleHttpException err =
      return $ Left $ NetworkError ("Network error: " ++ displayException err)

    handleGenericException :: SomeException -> IO (Either DriveError a)
    handleGenericException err =
      return $ Left $ NetworkError ("Unexpected error: " ++ displayException err)
```

### Error Logging

All functions log errors before returning:

```haskell
uploadFile config token local drive = do
  result <- uploadFileInternal config token local drive
  case result of
    Left err -> do
      logDriveError err
      return $ Left err
    Right fileId -> do
      logSuccess fileId
      return $ Right fileId
  where
    logDriveError (AuthError msg) =
      putStrLn $ "[AUTH ERROR] " ++ msg
    logDriveError (NetworkError msg) =
      putStrLn $ "[NETWORK ERROR] " ++ msg
    logDriveError (FileNotFound path) =
      putStrLn $ "[ERROR] File not found: " ++ path
    logDriveError InvalidTokens =
      putStrLn "[ERROR] Invalid tokens - run auth-setup again"

    logSuccess (DriveFileId fid) =
      putStrLn $ "[SUCCESS] Uploaded to Drive: " ++ fid
```

### Token Refresh Error Scenarios

| Scenario | Error Code | Behavior |
|----------|------------|----------|
| **Refresh token valid** | 200 | Success, return new access token |
| **Refresh token expired** | 401, invalid_grant | Return `InvalidTokens`, user must re-authenticate |
| **Network timeout** | HttpException | Return `NetworkError`, keep local files |
| **Malformed response** | Parse error | Return `NetworkError`, log details |
| **Rate limited** | 429 | Return `NetworkError` with retry message |

## Testing Considerations

### Manual Testing Checklist

1. **First-time auth setup**:
   - Place `client_secret.json` in `~/.mangascraper/`
   - Run `MangaScraper-exe auth-setup`
   - Verify browser opens (or URL displayed)
   - Complete OAuth flow
   - Verify `google_tokens.json` created with valid tokens

2. **Token refresh**:
   - Manually set token expiry to past time in JSON
   - Upload a file
   - Verify auto-refresh occurs
   - Verify new expiry time in updated JSON

3. **Folder creation**:
   - Upload chapter for new manga
   - Verify folder hierarchy created in Drive
   - Upload another chapter to same manga
   - Verify folder reused (check via cache or Drive UI)

4. **File upload**:
   - Upload CBZ file
   - Verify appears in correct Drive folder
   - Check file metadata (name, size, mime type)

5. **Error scenarios**:
   - Remove `client_secret.json` → verify auth error
   - Delete `google_tokens.json` → verify InvalidTokens
   - Disconnect network → verify upload fails gracefully
   - Manually revoke token in Google settings → verify re-auth required

### Error Injection for Testing

```haskell
-- In development, can mock token expiry:
testExpiredToken :: Tokens -> Tokens
testExpiredToken tokens = tokens { tokensExpiry = addUTCTime (-3600) now }

-- Mock network failure:
mockNetworkFailure :: IO a -> IO a
mockNetworkFailure = const $ throwM $ HttpExceptionRequest req ConnectionTimeout
```

## Implementation Steps

1. Update imports in `GoogleDrive.hs`
2. Implement `loadOAuthClient` function
3. Implement token conversion helpers
4. Replace `refreshAccessToken` stub
5. Implement `newDriveEnv` helper
6. Implement folder search and creation
7. Implement file upload
8. Update `ensureFolderPath` to use new implementations
9. Update `uploadFile` to use new implementations
10. Add error catching wrapper
11. Add `auth-setup` command to `Main.hs`
12. Test OAuth flow manually
13. Test file upload end-to-end

## Dependencies Already Added

From previous session:
```cabal
, gogol >= 1.0 && < 1.1
, gogol-core >= 1.0 && < 1.1
, gogol-drive >= 1.0 && < 1.1
```

These provide all necessary OAuth and Drive API functionality.

## Security Considerations

- OAuth scope limited to `drive.file` (only files created by app)
- Token file at `~/.mangascraper/google_tokens.json` should be chmod 600
- Client secret should not be committed to git (already in `.gitignore`)
- Refresh tokens stored securely, never logged
- Access tokens ephemeral (1-hour lifetime)

## Future Enhancements (Out of Scope)

- Exponential backoff retry logic
- Background upload queue for resilience
- Batch operations for multiple files
- Progress reporting for large uploads
- Support for resumable uploads (>5MB files)
- Token encryption at rest
- OAuth device flow (no browser required)
