{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Infra
  where
import System.Log.FastLogger

data Env = Env {
  logFunc :: LogStr -> IO()
  }
