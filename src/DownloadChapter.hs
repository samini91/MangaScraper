{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}

module DownloadChapter
  (
    download
  )
where 

import Path 
import ScraperData
import Text.HTML.TagSoup
import Text.HTML.Scalpel
import Control.Concurrent.Async
import System.Directory
import Network.Wreq
import Control.Lens
import Scraper
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B

download :: DownloadChapterRequest -> IO DownloadInfo
download u = do
  m <- grabPageRemoveRedundancy $ getMangaWebSiteUrl <$> cycle(link u) -- why doesnt this work?
  let z = snd $ m
  let strHtml = T.unpack $ z
  x <-  return (parseImages (fst m) (parseTags strHtml))
  _ <-  async $ saveFiles (downloadInfoUrl x) -- there is probably a better way to do this
  return (DownloadInfo {downloadInfoUrl = (downloadInfoUrl x)})
  where
      filePath = mangaFilePath (downloadChapterRequestMangaName u) chapterNumber 
      saveWithPath = saveFile (filePath)
      saveFiles y = Prelude.mapM saveWithPath (filter (\x -> urlValue (snd x) /= "http://")  y) 
      chapterNumber = show (downloadChapterRequestNumber u) ++ "_" ++ (pageLinkChapterName $ (head $ downloadChapterRequestLink (u))) -- use nonempty list... this can blow up
      link r= (pageLinkUrl <$> (downloadChapterRequestLink r))


saveFile :: Maybe(Path Rel Dir)-> (PageNumber, Url) -> IO FilePath -- pass in the path instead 
saveFile p x = do
  m <- get (urlValue $ snd x)
  let z = (m ^. responseBody)
  createDirectoryIfMissing True filePath -- get string representation here return FilePath of "" if maybe is empty
  B.writeFile fileName z
  return filePath
  where
    filePath = case (fromRelDir <$> p) of
                 Nothing -> ""
                 Just a -> a
    fileName = filePath ++ (show $ fst x) ++ ".jpg" -- should wrap this in a monad too

-- katana
parseImagesKatana :: [Tag String] -> DownloadInfo
parseImagesKatana tags = do
  let x = scrapeImages
  DownloadInfo {downloadInfoUrl = scrapeImagesWithPageNumber}
  where
    s = scrape (attrs "data-src" "img") tags
    scrapeImagesWithPageNumber = ( zip [0..] scrapeImages)  
    scrapeImages = case s of Just a -> sanatizeUrl <$> a
                             Nothing -> []

-- kakalot
parseImagesKakalot :: [Tag String] -> DownloadInfo
parseImagesKakalot tags = do
  let x = scrapeImages
  DownloadInfo {downloadInfoUrl = scrapeImagesWithPageNumber}
  where
    s = scrape (attrs "src" "img") tags
    scrapeImagesWithPageNumber = ( zip [0..] scrapeImages)  
    scrapeImages = case s of Just a -> sanatizeUrl <$> a
                             Nothing -> []


parseImages :: MangaWebSite -> [Tag String] -> DownloadInfo
parseImages (MangaKatana _) l = parseImagesKatana l
parseImages (MangaKakalot _) l = parseImagesKakalot l
                             
mangaFilePath :: String -> String -> Maybe (Path Rel Dir)
mangaFilePath a b = do
  x <- (parseRelDir "manga") 
  y <- (parseRelDir a)
  z <- (parseRelDir b)
  return (x </> y </> z)  



