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
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Manga
  (
    getMangaLinks,
    getMangaLinksHandler,
    downloadMangaHandler
  )
  where
import qualified Data.Text as T
import Servant (Handler)
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Text.HTML.TagSoup
import Scraper
import ScraperData
import DownloadChapter
import Data.List
import Data.Char
import Control.Concurrent.Async (mapConcurrently_, mapConcurrently)
import Control.Concurrent (forkIO)
import System.Log.FastLogger
import qualified ScraperData as SD
import Utils (mapConcurrentlyWithLimit)
import Infra (Env(..))

downloadMangaHandler :: Env -> PageLinkRequest -> Handler [PageLink]
downloadMangaHandler env link = liftIO $ downloadManga env link

downloadManga :: Env -> PageLinkRequest -> IO[PageLink]
downloadManga env links = do
  let logger = newFastLogger (LogStdout 100)
  l <- getMangaLinks env links
  let chapters = zip [0..] l -- this can be better
  let m = map (\z -> DownloadChapterRequest{
                           downloadChapterRequestMangaName = pageLinkRequestMangaName links,
                           downloadChapterRequestNumber = fst z,
                           downloadChapterRequestLink = snd z}) chapters
  let logList = intercalate "\n" (show . SD.pageLinkUrl <$> l)
  _ <- env.logFunc $ toLogStr logList
  _ <- downloadAllm m
  return l
  where
    downloadAllm x = mapConcurrentlyWithLimit (download env) x 5

getMangaLinksHandler :: Env -> PageLinkRequest -> Handler [PageLink]
getMangaLinksHandler env r = liftIO $ getMangaLinks env r

getMangaLinks :: Env -> PageLinkRequest -> IO [PageLink]
getMangaLinks env r = do
  x <- extractMangaPages (pageLinkRequestMangaName r) $ sanatizeUrl $ pageLinkRequestUrl r
--  let logList = intercalate "\n" (show . SD.pageLinkUrl <$> concat x)
--  _ <- env.logFunc $ toLogStr logList
  return x
--  where
--      comparison = (\(PageLink _ _ name1) (PageLink _ _ name2)  -> compare name1 name2)

type MangaName = String
extractMangaPages :: MangaName -> Url -> IO [PageLink]
extractMangaPages name x =
  do
  (site, html) <- grabPageMangaChapterLinks (mangaWebSite x)
  --let strHtml = return (fst z , T.unpack $ snd z)
  let e = parseAllChapters site (parseTags (T.unpack html)) -- pattern match what parser we want to use
  --return $ mergePageLinks $ toPageLink <$> e
  return $ reverse $ toPageLink e
  where
    toPageLink m = map (\z -> PageLink{ pageLinkUrl = (webSite $ (fst z)) , pageLinkChapterName = (snd z), pageLinkMangaName = name }) m
    --toPageLink m = PageLink{ pageLinkUrl = (webSite $ (fst m)) , pageLinkChapterName = (snd m), pageLinkMangaName = name }
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



