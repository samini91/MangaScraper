{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

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
  , ensureFolderPath
  , uploadFile
  ) where

import GHC.Generics
import Data.Aeson
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import System.Directory (getHomeDirectory, createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), takeDirectory, splitDirectories)
import Control.Exception (catch, SomeException)
import qualified Data.Map.Strict as Map
import Data.IORef

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

-- | Load tokens from disk
loadTokens :: FilePath -> IO (Either DriveError Tokens)
loadTokens = undefined

-- | Save tokens to disk
saveTokens :: FilePath -> Tokens -> IO ()
saveTokens = undefined

-- | Refresh access token using refresh token
refreshAccessToken :: FilePath -> RefreshToken -> IO (Either DriveError AccessToken)
refreshAccessToken = undefined

-- | Ensure folder path exists in Google Drive, creating hierarchy as needed
ensureFolderPath :: DriveConfig -> AccessToken -> FilePath -> IO (Either DriveError FolderId)
ensureFolderPath = undefined

-- | Upload file to Google Drive
uploadFile :: DriveConfig -> AccessToken -> FilePath -> FilePath -> IO (Either DriveError DriveFileId)
uploadFile = undefined
