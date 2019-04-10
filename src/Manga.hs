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

getMangaLinksHandler :: PageLinkRequest -> Handler [[PageLink]]
getMangaLinksHandler r = liftIO $ getMangaLinks r

getMangaLinks :: PageLinkRequest -> IO [[PageLink]]
getMangaLinks r = do
  x <- extractMangaPages (pageLinkRequestMangaName r) $ sanatizeUrl <$> pageLinkRequestUrl r
  return $ reverse $ x

type MangaName = String
extractMangaPages :: MangaName -> [Url] -> IO [[PageLink]] 
extractMangaPages name x =
  do
  z <-  grabPageMangaChapterLinks (mangaWebSite <$> x)
  strHtml <- return $ z >>= (\q -> return $ (fst q , T.unpack $ snd q))
  e <- return $ (\m -> ((parseAllChapters (fst m) ) $ parseTags (snd m))) <$> strHtml -- pattern match what parser we want to use
  return $ mergePageLinks $ toPageLink <$> e 
  where
    toPageLink m = (map (\z -> PageLink{ pageLinkUrl = (webSite $ (fst z)) , pageLinkChapterName = (snd z), pageLinkMangaName = name }) m)
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



