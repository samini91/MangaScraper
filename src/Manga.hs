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
import Control.Monad
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
import Data.List
import Data.Char

downloadMangaHandler :: PageLinkRequest -> Handler [[PageLink]]
downloadMangaHandler links = liftIO $ downloadManga links

downloadManga :: PageLinkRequest -> IO[[PageLink]]
downloadManga links = do
  l <- getMangaLinks links
  let chapters = zip [0..] l -- this can be better
  m <- return (map (\z -> DownloadChapterRequest{
                       downloadChapterRequestMangaName = pageLinkRequestMangaName links,
                       downloadChapterRequestNumber = (fst z) ,
                       downloadChapterRequestLink = (snd z)}) chapters)
  _ <- downloadAllm m
  return l
  where
    downloadAllm x = Prelude.mapM download x

-- grabLinksHandler :: PageLinkRequest -> Handler [PageLink]
-- grabLinksHandler r = liftIO $ grabLinks r

getMangaLinksHandler :: PageLinkRequest -> Handler [[PageLink]]
getMangaLinksHandler r = liftIO $ getMangaLinks r

--getMangaLinks :: PageLinkRequest -> IO [PageLink]
getMangaLinks :: PageLinkRequest -> IO [[PageLink]]
getMangaLinks r = do
  x <- doit $ sanatizeUrl <$> pageLinkRequestUrl r
  return $ reverse $ x

-- grabLinks :: PageLinkRequest -> IO [PageLink]
-- grabLinks r = do
--   --z <- grabPageWithRetry' $ urls -- short circut out of this via transformer? how
--   z <- grabPageRemoveRedundancy $ urls  -- short circut out of this via transformer? how
--   --let strHtml = T.unpack $ snd $ listDefault z
--   let strHtml = T.unpack $ snd $ z
--   --let webSite = getWebSite $ pageLinkRequestUrl z
--   m <- return (parseChapters  (parseTags strHtml))
--   return (map (\x -> PageLink{ pageLinkUrl = (webSite $ (fst x)) , pageLinkChapterName = (snd x), pageLinkMangaName = pageLinkRequestMangaName r}) m)
--     where
--     urls = map sanatizeUrl $ pageLinkRequestUrl r
--   -- shitty version of safe head
--     listDefault a = case a of [] -> (getDefaultMangaWebSite, "")
--                               (h:_) -> h
--   -- More shittyness... gonna fix this with getting good at embedded maybet
--     webSite s = case (getMangaWebSite s) of Just a -> a
--                                             Nothing -> getDefaultMangaWebSite

--         --url = case (pageLinkRequestUrl r) of MangaKatana s -> s
--         --                                 Mangakakalot s -> s
      


--grabLinks' :: PageLinkRequest -> IO [[PageLink]]
--grabLinks' links = do
--  z <- return $ urls
--  --return (map (\x -> PageLink{ pageLinkUrl = (webSite $ (fst x)) , pageLinkChapterName = (snd x), pageLinkMangaName = pageLinkRequestMangaName r}) m)
--  --return []
--  
--  where
--    urls = sanatizeUrl <$> pageLinkRequestUrl links
--    webSite s = case (getMangaWebSite s) of Just a -> a
--                                            Nothing -> getDefaultMangaWebSite
--

-- doit' :: [Url] -> IO [[PageLink]]
-- doit' x =
--   do
--   z <- grabPages x
--   strHtml <- return $ T.unpack <$> z
--   e <- return $ (parseChapters . parseTags) <$> strHtml
--   return $ toPageLink <$> e
--   where
--     toPageLink m = (map (\x -> PageLink{ pageLinkUrl = (webSite $ (fst x)) , pageLinkChapterName = (snd x)}) m)
--     webSite s = case (getMangaWebSite s) of Just a -> a
--                                             Nothing -> getDefaultMangaWebSite
  
--doit :: [Url] -> IO [[(MangaWebeSite, PageLink)]]
doit :: [Url] -> IO [[PageLink]] 
doit x =
  do
    --z <-  grabPages x
  z <-  grabPageMangaChapterLinks (mangaWebSite <$> x)
  --let mangaWebSite = getMangaWebSiteWithUrl 
  --strHtml <- return $ T.unpack <$> snd <$> z
  strHtml <- return $ z >>= (\q -> return $ (fst q , T.unpack $ snd q))
  --e <- return $ ((parseAllChapters (fst strHtml) ) . parseTags) <$> snd <$> strHtml -- pattern match what parser we want to use
  e <- return $ (\x -> ((parseAllChapters (fst x) ) $ parseTags (snd x))) <$> strHtml -- pattern match what parser we want to use
  --return $ toPageLink <$> e -- call merge here
  return $ mergePageLinks $ toPageLink <$> e -- call merge here
  where
    toPageLink m = (map (\x -> PageLink{ pageLinkUrl = (webSite $ (fst x)) , pageLinkChapterName = (snd x), pageLinkMangaName = "TEMP" }) m)
    webSite s = case (getMangaWebSite s) of Just a -> a
                                            Nothing -> getDefaultMangaWebSite
    mangaWebSite s = case (getMangaWebSiteWithUrl s) of Just a -> a
                                                        Nothing -> getDefaultMangaWebSite


mergePageLinks :: [[PageLink]] -> [[PageLink]]
mergePageLinks x = groupBy grouping $ sortBy comparison (join x)
  where
    comparison = (\(PageLink _ _ name1) (PageLink _ _ name2)  -> compare name1 name2)
    grouping = (\(PageLink _ _ name1) (PageLink _ _ name2) -> compareChapterNames name1 name2)
    compareChapterNames s1 s2 = getChapterNumber (filter isDigit s1) == getChapterNumber (filter isDigit s2) 
    getChapterNumber :: String -> Integer 
    getChapterNumber s = read s  -- why isnt this forced into a maybe since this can fail?
--mergePageLinks [] = []
--mergePageLinks [x] = [x] -- if we only have one list inside the list return to avoid redundant computing


