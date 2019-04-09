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
import Scraper
import Manga
import ScraperData
import DownloadChapter

type API = "downloadManga" :> ReqBody '[JSON] PageLinkRequest :> Post '[JSON] [[PageLink]]
           
startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Servant.Proxy API
api = Servant.Proxy

server :: Server API
server = downloadMangaHandler

