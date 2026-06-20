{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DataKinds #-}

module GoogleDrive
  ( DriveConfig(..)
  , Tokens(..)
  , DriveError(..)
  , AccessToken(..)
  , RefreshToken(..)
  , FolderId(..)
  , DriveFileId(..)
  , loadTokens
  , saveTokens
  , refreshAccessToken
  , isTokenExpired
  , getValidToken
  , ensureFolderPath
  , uploadFile
  , performInitialAuth
  , loadOAuthClient
  , authorizedUserFromTokens
  , tokensFromOAuthToken
  ) where

import GHC.Generics
import Data.Aeson
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import System.Directory (getHomeDirectory, createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), takeDirectory, splitDirectories)
import Control.Exception (SomeException)
import qualified Data.Map.Strict as Map
import Data.IORef
import qualified Data.ByteString as BS
import Control.Monad (void)
import Control.Monad.Catch (catch, throwM, try)
import Control.Monad.Trans.Resource (runResourceT)
import System.Info (os)
import System.Process (rawSystem)
import System.Exit (ExitCode(..))
import System.IO (hFlush, stdout)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Google
  ( Env
  , newEnv
  , send
  , upload
  , toBody
  )
import Network.Google.Auth
  ( Credentials(..)
  , OAuthClient(..)
  , OAuthCode(..)
  , OAuthToken(..)
  , ClientId(..)
  , ClientSecret(..)
  , _tokenAccess
  , _tokenRefresh
  , _tokenExpiry
  )
import qualified Network.Google.Auth
import Network.Google.Auth.InstalledApplication
  ( installedApplication
  , formURL
  , AccessType(..)
  , exchangeCode
  )
import Network.Google.Auth.ServiceAccount (authorizedUser, AuthorizedUser(..))
import Network.Google.Drive
  ( DriveScopes
  , filesList
  , filesCreate
  , file
  )
import Network.Google.Drive.Types
  ( File
  , FileList
  , flFiles
  , flQ
  , flPageSize
  , fId
  , fName
  , fMimeType
  , fParents
  )
import Data.Proxy (Proxy(..))
import Lens.Micro ((&), (^?), (^.), (.~))
import Data.Aeson.Lens (key, _String)

-- | Configuration for Google Drive integration
data DriveConfig = DriveConfig
  { driveConfigClientSecretPath :: FilePath
  , driveConfigTokenPath :: FilePath
  , driveConfigRootFolderId :: Maybe String
  , driveFolderCache :: IORef (Map.Map FilePath FolderId)
  } deriving (Generic)

-- | OAuth tokens
data Tokens = Tokens
  { tokensAccess :: AccessToken
  , tokensRefresh :: RefreshToken
  , tokensExpiry :: UTCTime
  } deriving (Generic, Show)

instance FromJSON Tokens where
  parseJSON = withObject "Tokens" $ \v -> Tokens
    <$> (AccessToken <$> v .: "access_token")
    <*> (RefreshToken <$> v .: "refresh_token")
    <*> v .: "expiry"

instance ToJSON Tokens where
  toJSON (Tokens (AccessToken a) (RefreshToken r) e) = object
    [ "access_token" .= a
    , "refresh_token" .= r
    , "expiry" .= e
    ]

-- | Access token wrapper
newtype AccessToken = AccessToken T.Text deriving (Generic, Show, Eq)

-- | Refresh token wrapper
newtype RefreshToken = RefreshToken T.Text deriving (Generic, Show, Eq)

-- | Folder ID wrapper
newtype FolderId = FolderId String deriving (Generic, Show, Eq, Ord)

-- | Drive file ID wrapper
newtype DriveFileId = DriveFileId String deriving (Generic, Show, Eq)

-- | Drive operation errors
data DriveError
  = AuthError String
  | NetworkError String
  | FileNotFound FilePath
  | InvalidTokens
  deriving (Show, Eq)

-- | Check if token is expired
isTokenExpired :: Tokens -> IO Bool
isTokenExpired tokens = do
  now <- getCurrentTime
  return $ now >= tokensExpiry tokens

-- | Load tokens from disk
loadTokens :: FilePath -> IO (Either DriveError Tokens)
loadTokens tokenPath = do
  exists <- doesFileExist tokenPath
  if not exists
    then return $ Left InvalidTokens
    else do
      result <- catch (Right <$> BL.readFile tokenPath) handleIOException
      case result of
        Left err -> return $ Left err
        Right content -> case eitherDecode content of
          Left err -> return $ Left $ AuthError ("Failed to parse tokens: " ++ err)
          Right tokens -> return $ Right tokens
  where
    handleIOException :: SomeException -> IO (Either DriveError BL.ByteString)
    handleIOException e = return $ Left $ AuthError ("Failed to read token file: " ++ show e)

-- | Save tokens to disk
saveTokens :: FilePath -> Tokens -> IO ()
saveTokens tokenPath tokens = do
  let tokenDir = takeDirectory tokenPath
  createDirectoryIfMissing True tokenDir
  BL.writeFile tokenPath (encode tokens)
  -- Note: In production, should set file permissions to 0600 for security
  return ()

-- | Refresh access token using refresh token
refreshAccessToken :: FilePath -> RefreshToken -> IO (Either DriveError AccessToken)
refreshAccessToken clientSecretPath (RefreshToken refreshToken) = do
  -- 1. Load OAuth client
  clientResult <- loadOAuthClient clientSecretPath

  case clientResult of
    Left err -> return $ Left err
    Right (OAuthClient clientId clientSecret) -> do
      -- 2. Create AuthorizedUser from stored refresh token
      let authUser = AuthorizedUser
            { _userId = clientId
            , _userSecret = clientSecret
            , _userRefresh = Gogol.Types.RefreshToken refreshToken
            }

      -- 3. Request new access token
      manager <- newManager tlsManagerSettings

      tokenResult <- try $ authorizedUserToken authUser Nothing logger manager

      case tokenResult of
        Left (err :: SomeException) ->
          return $ Left $ AuthError ("Token refresh failed: " ++ show err)

        Right (OAuthToken (Gogol.Types.AccessToken accessTokenText) _refreshToken _expiry) -> do
          -- Extract the access token Text from OAuthToken
          -- Our custom AccessToken wraps the Text
          let newAccessToken = AccessToken accessTokenText
          return $ Right newAccessToken
  where
    logger _ _ = return ()  -- Silent logger for refresh operations

-- | Load tokens and refresh if expired
getValidToken :: DriveConfig -> IO (Either DriveError AccessToken)
getValidToken config = do
  tokensResult <- loadTokens (driveConfigTokenPath config)
  case tokensResult of
    Left err -> return $ Left err
    Right tokens -> do
      expired <- isTokenExpired tokens
      if expired
        then do
          refreshResult <- refreshAccessToken (driveConfigClientSecretPath config) (tokensRefresh tokens)
          case refreshResult of
            Left err -> return $ Left err
            Right newAccessToken -> do
              -- Update expiry to 1 hour from now
              now <- getCurrentTime
              let newExpiry = addUTCTime 3600 now
              let updatedTokens = tokens { tokensAccess = newAccessToken, tokensExpiry = newExpiry }
              saveTokens (driveConfigTokenPath config) updatedTokens
              return $ Right newAccessToken
        else return $ Right (tokensAccess tokens)

-- | Search for folder by name under parent
searchFolder :: Env -> T.Text -> FolderId -> IO (Maybe FolderId)
searchFolder env folderName (FolderId parentId) = do
  let query = T.concat
        [ "name = '", folderName, "'"
        , " and mimeType = 'application/vnd.google-apps.folder'"
        , " and '", T.pack parentId, "' in parents"
        , " and trashed = false"
        ]

  let request = filesList
        & flQ .~ Just query
        & flPageSize .~ Just 1
  response <- runResourceT $ send env request

  case response ^. flFiles of
    Just (file:_) -> return $ fmap (FolderId . T.unpack) (file ^. fId)
    _ -> return Nothing

-- | Create new folder under parent
createFolderInParent :: Env -> T.Text -> FolderId -> IO (Either DriveError FolderId)
createFolderInParent env folderName (FolderId parentId) = catchDriveErrors $ do
  let metadata = file
        & fName .~ Just folderName
        & fMimeType .~ Just "application/vnd.google-apps.folder"
        & fParents .~ Just [T.pack parentId]

  created <- runResourceT $ send env (filesCreate metadata)

  case created ^. fId of
    Just fileId -> return $ FolderId (T.unpack fileId)
    Nothing -> throwM $ userError "Failed to get folder ID from response"

-- | Create folder hierarchy recursively
createFolderHierarchy :: DriveConfig -> AccessToken -> FolderId -> [FilePath] -> IO (Either DriveError FolderId)
createFolderHierarchy _ _ parentId [] = return $ Right parentId
createFolderHierarchy config (AccessToken _) parentId (folderName:rest) = do
  -- 1. Load credentials and create env
  clientResult <- loadOAuthClient (driveConfigClientSecretPath config)
  case clientResult of
    Left err -> return $ Left err
    Right oauthClient -> do
      tokensResult <- loadTokens (driveConfigTokenPath config)
      case tokensResult of
        Left err -> return $ Left err
        Right toks -> do
          let authUser = authorizedUserFromTokens toks oauthClient
          manager <- newManager tlsManagerSettings
          env <- newDriveEnv authUser manager

          -- 2. Search for existing folder
          existing <- searchFolder env (T.pack folderName) parentId

          -- 3. Use existing or create new
          folderIdResult <- case existing of
            Just fid -> return $ Right fid
            Nothing -> createFolderInParent env (T.pack folderName) parentId

          -- 4. Recurse for remaining path components
          case folderIdResult of
            Left err -> return $ Left err
            Right folderId ->
              if null rest
                then return $ Right folderId
                else createFolderHierarchy config (AccessToken "") folderId rest

-- | Ensure folder path exists in Google Drive, creating hierarchy as needed
ensureFolderPath :: DriveConfig -> AccessToken -> FilePath -> IO (Either DriveError FolderId)
ensureFolderPath config accessToken path = do
  -- Check cache first
  cache <- readIORef (driveFolderCache config)
  case Map.lookup path cache of
    Just folderId -> return $ Right folderId
    Nothing -> do
      -- Split path into components
      let pathComponents = splitDirectories path
      -- Start from root or configured root folder
      let rootFolderId = maybe "root" id (driveConfigRootFolderId config)
      -- Create folders one by one
      result <- createFolderHierarchy config accessToken (FolderId rootFolderId) pathComponents
      case result of
        Right folderId -> do
          -- Cache the result
          modifyIORef' (driveFolderCache config) (Map.insert path folderId)
          return $ Right folderId
        Left err -> return $ Left err

-- | Upload file content to Drive
uploadFileContent :: Env -> FilePath -> T.Text -> FolderId -> IO (Either DriveError DriveFileId)
uploadFileContent env localPath fileName (FolderId parentId) = catchDriveErrors $ do
  -- 1. Read file content
  fileContent <- BS.readFile localPath

  -- 2. Create metadata
  let metadata = file
        & fName .~ Just fileName
        & fParents .~ Just [T.pack parentId]
        & fMimeType .~ Just "application/x-cbz"

  -- 3. Upload with multipart
  uploaded <- runResourceT $ upload env (filesCreate metadata) (toBody fileContent)

  -- 4. Extract file ID
  case uploaded ^. fId of
    Just fileId -> return $ DriveFileId (T.unpack fileId)
    Nothing -> throwM $ userError "Failed to get file ID from upload response"

-- | Upload file to Google Drive
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
              env <- newDriveEnv authUser manager

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

-- | Placeholder for initial OAuth authentication flow
-- TODO: Implement in Task 2
performInitialAuth :: IO (Either DriveError Tokens)
performInitialAuth = return $ Left $ AuthError "performInitialAuth not yet implemented"

-- | Load OAuth client credentials from client_secret.json
loadOAuthClient :: FilePath -> IO (Either DriveError OAuthClient)
loadOAuthClient path = do
  exists <- doesFileExist path
  if not exists
    then return $ Left $ AuthError "Client secret file not found"
    else do
      result <- catch (Right <$> BL.readFile path) handleIOException
      case result of
        Left err -> return $ Left err
        Right content -> do
          case eitherDecode content of
            Left err -> return $ Left $ AuthError ("Invalid client_secret.json: " ++ err)
            Right (jsonValue :: Value) -> do
              let installed = jsonValue ^? key "installed"
              let clientId = installed >>= (^? key "client_id" . _String)
              let clientSecret = installed >>= (^? key "client_secret" . _String)
              case (clientId, clientSecret) of
                (Just cid, Just csec) ->
                  return $ Right $ OAuthClient (ClientId cid) (GSecret csec)
                _ -> return $ Left $ AuthError "Missing client_id or client_secret in installed section"
  where
    handleIOException :: SomeException -> IO (Either DriveError BL.ByteString)
    handleIOException e = return $ Left $ AuthError ("Failed to read client secret: " ++ show e)

-- | Convert custom Tokens and OAuthClient to gogol's AuthorizedUser
authorizedUserFromTokens :: Tokens -> OAuthClient -> AuthorizedUser
authorizedUserFromTokens (Tokens _ (RefreshToken refresh) _) (OAuthClient clientId clientSecret) =
  AuthorizedUser
    { _userId = clientId
    , _userSecret = clientSecret
    , _userRefresh = Gogol.Types.RefreshToken refresh
    }

-- | Convert gogol's tokens to custom Tokens
-- Takes access token text, optional refresh token text, and expiry time
tokensFromOAuthToken :: OAuthToken -> Tokens
tokensFromOAuthToken oauthToken =
  Tokens
    { tokensAccess = accessTokenFromGogol (_tokenAccess oauthToken)
    , tokensRefresh = maybe (RefreshToken "") refreshTokenFromGogol (_tokenRefresh oauthToken)
    , tokensExpiry = _tokenExpiry oauthToken
    }

-- Helper to convert from gogol's AccessToken to our custom AccessToken
accessTokenFromGogol :: Network.Google.Auth.AccessToken -> AccessToken
accessTokenFromGogol (Network.Google.Auth.AccessToken txt) = AccessToken txt

-- Helper to convert from gogol's RefreshToken to our custom RefreshToken
refreshTokenFromGogol :: Network.Google.Auth.RefreshToken -> RefreshToken
refreshTokenFromGogol (Network.Google.Auth.RefreshToken txt) = RefreshToken txt

-- | Create Google Drive API environment from credentials
newDriveEnv :: AuthorizedUser -> Manager -> IO Env
newDriveEnv authUser manager = do
  let credentials = FromUser authUser
  let logger _ _ = return ()
  env <- newEnvWith credentials logger manager
  return env

-- | Catch Drive API exceptions and convert to DriveError
catchDriveErrors :: IO a -> IO (Either DriveError a)
catchDriveErrors action =
  (Right <$> action)
    `catch` handleGenericException
  where
    handleGenericException :: SomeException -> IO (Either DriveError a)
    handleGenericException err =
      return $ Left $ NetworkError ("Drive API error: " ++ show err)
