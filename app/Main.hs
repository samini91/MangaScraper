module Main where

import Lib (startApp)
import qualified GoogleDrive as GD
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Directory (getHomeDirectory, createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import System.IO (hFlush, stdout)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock (getCurrentTime, addUTCTime)
import Control.Monad.Catch (try, SomeException)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Google.Auth (OAuthCode(..), OAuthClient(..), OAuthToken(..))
import Network.Google.Auth.InstalledApplication (formURL, AccessType(..), exchangeCode)
import Network.Google.Drive (driveFileScope)
import qualified System.Info
import qualified System.Process

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

    Right client -> do
      -- 2. Generate authorization URL
      let url = formURL client [driveFileScope]

      putStrLn "Opening browser for authorization..."
      putStrLn $ "URL: " ++ T.unpack url
      putStrLn ""

      -- Try to open browser (optional, user can manually copy URL)
      _ <- tryOpenBrowser url

      -- 3. Get authorization code from user
      putStrLn "Please authorize the application and paste the code here:"
      putStr "Authorization code: "
      hFlush stdout
      codeText <- T.strip <$> T.getLine

      -- 4. Exchange code for tokens
      let oauthCode = OAuthCode codeText

      -- 5. Get initial token
      manager <- newManager tlsManagerSettings

      tokenResult <- try $ exchangeCode client oauthCode (\_ _ -> return ()) manager

      case tokenResult of
        Left (err :: SomeException) -> do
          putStrLn ""
          putStrLn "ERROR: Failed to exchange code for tokens"
          putStrLn $ "  " ++ show err
          putStrLn ""
          putStrLn "Please verify:"
          putStrLn "  - The authorization code is correct"
          putStrLn "  - You haven't used this code already"
          putStrLn "  - Your client_secret.json is valid"
          exitFailure

        Right oauthToken -> do
          -- 6. Extract and save tokens using helper function
          let tokens = GD.tokensFromOAuthToken oauthToken

          let tokenPath = home </> ".mangascraper/google_tokens.json"
          -- Ensure directory exists
          createDirectoryIfMissing True (takeDirectory tokenPath)
          GD.saveTokens tokenPath tokens

          putStrLn ""
          putStrLn "✓ Authentication successful!"
          putStrLn $ "Tokens saved to: " ++ tokenPath
          putStrLn ""
          putStrLn "You can now use MangaScraper with Google Drive integration."

-- | Try to open URL in browser
tryOpenBrowser :: T.Text -> IO ()
tryOpenBrowser url = do
  let os = System.Info.os
  void $ case os of
    "darwin" -> System.Process.rawSystem "open" [T.unpack url]
    "linux"  -> System.Process.rawSystem "xdg-open" [T.unpack url]
    _        -> return System.Process.ExitSuccess  -- Windows or unknown, user copies manually
  where
    void :: IO a -> IO ()
    void = (>> return ())
