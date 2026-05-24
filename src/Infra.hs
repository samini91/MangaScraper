{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Infra
  where
import System.Log.FastLogger
import GoogleDrive (DriveConfig)

data Env = Env
  { logFunc :: LogStr -> IO ()
  , driveConfig :: Maybe DriveConfig
  }
