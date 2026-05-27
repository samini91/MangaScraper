module Main where

import Lib (startApp)
import qualified GoogleDrive as GD
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["auth-setup"] -> runAuthSetup
    _ -> startApp  -- existing behavior

runAuthSetup :: IO ()
runAuthSetup = do
  putStrLn "=== Google Drive Authentication Setup ==="
  putStrLn ""

  -- 1. Load client secret
  home <- getHomeDirectory
  let secretPath = home </> ".mangascraper/client_secret.json"
  clientResult <- GD.loadOAuthClient secretPath

  case clientResult of
    Left err -> do
      putStrLn $ "ERROR: " ++ show err
      putStrLn ""
      putStrLn "Please ensure client_secret.json exists at:"
      putStrLn $ "  " ++ secretPath
      putStrLn ""
      putStrLn "Download it from Google Cloud Console:"
      putStrLn "  https://console.cloud.google.com/apis/credentials"
      exitFailure

    Right _client -> do
      -- TODO: Task 9 - Complete OAuth flow implementation
      -- Need to:
      -- 1. Generate authorization URL using formAccessTypeURL
      -- 2. Open browser or display URL
      -- 3. Get authorization code from user
      -- 4. Exchange code for tokens using installedApplication
      -- 5. Save tokens to ~/.mangascraper/google_tokens.json

      putStrLn "ERROR: OAuth flow not yet fully implemented"
      putStrLn ""
      putStrLn "The auth-setup command structure is in place but requires:"
      putStrLn "  - formAccessTypeURL implementation (Task 9)"
      putStrLn "  - installedApplication flow (Task 9)"
      putStrLn "  - Token exchange logic (Task 9)"
      putStrLn ""
      putStrLn "Client secret loaded successfully from:"
      putStrLn $ "  " ++ secretPath
      exitFailure
