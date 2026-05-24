{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Network.Wai.Handler.Warp
import Servant
import Manga
import ScraperData
import System.Log.FastLogger
import Infra (Env (..))
import GoogleDrive (DriveConfig(..))
import System.Environment (lookupEnv)
import System.Directory (getHomeDirectory, doesFileExist)
import System.FilePath ((</>))
import qualified Data.Map.Strict as Map
import Data.IORef
import Data.Maybe (fromMaybe)

type API = "downloadManga" :> ReqBody '[JSON] PageLinkRequest :> Post '[JSON] [PageLink]

-- | Load Google Drive configuration from environment
loadDriveConfig :: IO (Maybe DriveConfig)
loadDriveConfig = do
  homeDir <- getHomeDirectory
  let defaultTokenPath = homeDir </> ".mangascraper/google_tokens.json"
  let defaultSecretPath = homeDir </> ".mangascraper/client_secret.json"

  -- Check for custom paths from environment
  customSecretPath <- lookupEnv "MANGASCRAPER_CLIENT_SECRET"
  let secretPath = fromMaybe defaultSecretPath customSecretPath

  -- Check if token file exists (indicates Drive is configured)
  tokenExists <- doesFileExist defaultTokenPath
  secretExists <- doesFileExist secretPath

  if tokenExists && secretExists
    then do
      folderCache <- newIORef Map.empty
      return $ Just $ DriveConfig
        { driveConfigClientSecretPath = secretPath
        , driveConfigTokenPath = defaultTokenPath
        , driveConfigRootFolderId = Nothing
        , driveFolderCache = folderCache
        }
    else return Nothing

startApp :: IO ()
startApp = do
  (logFunc,_) <- newFastLogger (LogStdout 100)
  maybeDriveConfig <- loadDriveConfig
  let env = Env
        { logFunc = logFunc
        , driveConfig = maybeDriveConfig
        }
  run 8080 (app env)
  
app :: Env -> Application
app env = serve api (server env)

api :: Servant.Proxy API
api = Servant.Proxy

-- use reader monad instead
server :: Env -> Server API
server = downloadMangaHandler 

