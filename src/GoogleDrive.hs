{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DisambiguateRecordFields #-}

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
import Gogol.Auth.TokenFile
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
import Gogol
  ( Env
  , newEnv
  , send
  , upload
  )
import qualified Gogol.Auth as GA
import Gogol.Auth
  ( Credentials(..)
  , OAuthClient(..)
  , OAuthCode(..)
  , ClientId(..)
  )
import Gogol.Auth.InstalledApplication
  ( installedApplication
  , formAccessTypeURL
  , AccessType(..)
  )
import Gogol.Auth.ServiceAccount (authorizedUserToken, AuthorizedUser(..))
import Gogol.Drive
  ( Drive'File
  , DriveFilesList
  , DriveFilesCreate
  , File
  , FileList
  , newDriveFilesList
  , newDriveFilesCreate
  , newFile
  )
import Gogol.Drive.Types
  ( File
  , file
  , FileList
  )
import Data.Proxy (Proxy(..))
import Lens.Micro ((^?), (^.), (.~))
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
  -- Read client secret file
  exists <- doesFileExist clientSecretPath
  if not exists
    then return $ Left $ AuthError "Client secret file not found"
    else do
      result <- catch (Right <$> BL.readFile clientSecretPath) handleIOException
      case result of
        Left err -> return $ Left err
        Right secretContent -> do
          -- Parse client secret to get client_id and client_secret
          case eitherDecode secretContent of
            Left err -> return $ Left $ AuthError ("Failed to parse client secret: " ++ err)
            Right (clientData :: Value) -> do
              -- For now, return a placeholder implementation
              -- In a real implementation, this would make an HTTP POST to
              -- https://oauth2.googleapis.com/token with refresh_token grant
              return $ Left $ AuthError "Token refresh not yet implemented - manual re-auth required"
  where
    handleIOException :: SomeException -> IO (Either DriveError BL.ByteString)
    handleIOException e = return $ Left $ AuthError ("Failed to read client secret: " ++ show e)

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

-- | Create folder hierarchy recursively
createFolderHierarchy :: DriveConfig -> AccessToken -> FolderId -> [FilePath] -> IO (Either DriveError FolderId)
createFolderHierarchy _ _ parentId [] = return $ Right parentId
createFolderHierarchy config accessToken parentId (folderName:rest) = do
  -- For now, return a placeholder error since we need actual Google Drive API calls
  -- In a real implementation, this would:
  -- 1. Search for folder with name under parentId
  -- 2. If found, use that folder ID
  -- 3. If not found, create new folder
  -- 4. Recurse with remaining path components
  return $ Left $ NetworkError "Folder creation not yet implemented - requires Google Drive API integration"

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

-- | Upload file to Google Drive
uploadFile :: DriveConfig -> AccessToken -> FilePath -> FilePath -> IO (Either DriveError DriveFileId)
uploadFile config accessToken localPath drivePath = do
  -- Check if local file exists
  exists <- doesFileExist localPath
  if not exists
    then return $ Left $ FileNotFound localPath
    else do
      -- Extract folder path and file name
      let driveFolder = takeDirectory drivePath
      let pathParts = splitDirectories drivePath
      let fileName = if null pathParts then "" else last pathParts

      -- Ensure folder exists
      folderResult <- ensureFolderPath config accessToken driveFolder
      case folderResult of
        Left err -> return $ Left err
        Right (FolderId parentId) -> do
          -- For now, return a placeholder error since we need actual Google Drive API calls
          -- In a real implementation, this would:
          -- 1. Read file content from localPath
          -- 2. Make multipart/related POST to Drive API v3
          -- 3. Include file metadata (name, parents) and file content
          -- 4. Return file ID from response
          return $ Left $ NetworkError "File upload not yet implemented - requires Google Drive API integration"

-- | Placeholder for initial OAuth authentication flow
-- TODO: Implement in Task 2
performInitialAuth :: IO (Either DriveError Tokens)
performInitialAuth = return $ Left $ AuthError "performInitialAuth not yet implemented"

-- | Placeholder for loading OAuth client configuration
-- TODO: Implement in Task 2
loadOAuthClient :: FilePath -> IO (Either DriveError OAuthClient)
loadOAuthClient _ = return $ Left $ AuthError "loadOAuthClient not yet implemented"
