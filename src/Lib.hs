{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
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

type API = "downloadManga" :> ReqBody '[JSON] PageLinkRequest :> Post '[JSON] [PageLink]
           
startApp :: IO ()
startApp = do
  (logFunc,_) <- newFastLogger (LogStdout 100)
  let env = Env {
        logFunc = logFunc
                }
  run 8080 (app env)
  
app :: Env -> Application
app env = serve api (server env)
--  where
--    env :: Env
--    env = Env {logger = newFastLogger (LogStdout 100)}

api :: Servant.Proxy API
api = Servant.Proxy

-- use reader monad instead
server :: Env -> Server API
server = downloadMangaHandler 

