{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}

module Manga
  (
    getMangaLinks,
    getMangaLinksHandler,
    downloadMangaHandler
  )
  where
import Data.Aeson
import Data.Aeson.TH
import qualified Data.Text as T
import Servant (Handler)
import GHC.Generics
import Control.Monad.IO.Class (liftIO)
import Control.Lens
import System.Directory
import Network.Wreq
import Text.HTML.TagSoup
import Network.Wreq
import qualified Data.ByteString.Lazy as B
import Text.HTML.Scalpel
import Test.WebDriver
import Test.WebDriver.Exceptions (FailedCommandType(Timeout))
import Test.WebDriver.Commands.Wait
import Test.WebDriver.Capabilities
import Test.WebDriver.Config
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Parallel as Par
import Control.Exception (Exception, catch, SomeException)
import Path 
import Path.Internal
import Scraper
import ScraperData
import DownloadChapter


downloadMangaHandler :: PageLinkRequest -> Handler [PageLink]
downloadMangaHandler links = liftIO $ downloadManga links

downloadManga :: PageLinkRequest -> IO[PageLink]
downloadManga links = do
  l <- getMangaLinks links
  let chapters = zip [0..] l
  m <- return (map (\z -> DownloadChapterRequest{
                       downloadChapterRequestMangaName = pageLinkRequestMangaName links,
                       downloadChapterRequestNumber = (fst z) ,
                       downloadChapterRequestLink = (snd z)}) chapters)
  _ <- downloadAllm m
  return l
  where
    downloadAllm x = Prelude.mapM download x

grabLinksHandler :: PageLinkRequest -> Handler [PageLink]
grabLinksHandler r = liftIO $ grabLinks r

getMangaLinksHandler :: PageLinkRequest -> Handler [PageLink]
getMangaLinksHandler r = liftIO $ getMangaLinks r

getMangaLinks :: PageLinkRequest -> IO [PageLink]
getMangaLinks r = do
  x <- grabLinks r
  return $ reverse x

grabLinks :: PageLinkRequest -> IO [PageLink]
grabLinks r = do
  z <- grabPageWithRetry urls
  let strHtml = T.unpack $ listDefault z
  m <- return (parseChapters (parseTags strHtml))
  return (map (\x -> PageLink{ pageLinkUrl = (sanatizeUrl (fst x)) , pageLinkChapterName = (snd x), pageLinkMangaName = pageLinkRequestMangaName r}) m)
    where
    urls = map sanatizeUrl $ pageLinkRequestUrl r
    listDefault a = case a of [] -> ""
                              (h:_) -> h
      --url = case (pageLinkRequestUrl r) of MangaKatana s -> s
        --                                 Mangakakalot s -> s
      

