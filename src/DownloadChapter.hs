{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}

module DownloadChapter
  (
    download
  )
where 

import Data.List
import Path 
import Path.Internal
import ScraperData
import Text.HTML.TagSoup
import Text.HTML.Scalpel
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent
import Control.Exception (SomeException)
import Control.Exception.Lifted
import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.CallStack
import Test.WebDriver
import Test.WebDriver.Exceptions (FailedCommandType(Timeout))
import Test.WebDriver.Commands.Wait
import Test.WebDriver.Capabilities
import Test.WebDriver.Config
import Text.HTML.TagSoup
import Text.HTML.Scalpel
import System.Directory
import Network.Wai
import Network.Wreq
import Network.HTTP
import Control.Lens
import ScraperData
import Scraper
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B

download :: DownloadChapterRequest -> IO DownloadInfo
download u = do
  z <- grabPageWithRetry $ [link]
  let strHtml = T.unpack $ listDefault $ take 1 $ takeWhile (\x -> x /= "") (cycle z)
  x <-  return (parseImagesFromString (parseTags strHtml))
  _ <- forkIO ((\dInfo -> ()) <$> (saveFiles (downloadInfoUrl x)))
  return (DownloadInfo {downloadInfoUrl = (downloadInfoUrl x)})
  where
      filePath = mangaFilePath (downloadChapterRequestMangaName u) chapterNumber 
      saveWithPath = saveFile (filePath)
      saveFiles y = Prelude.mapM saveWithPath (filter (\x -> urlValue (snd x) /= "http://")  y) 
      chapterNumber = show (downloadChapterRequestNumber u) ++ "_" ++ (pageLinkChapterName (downloadChapterRequestLink u))
      link = (pageLinkUrl (downloadChapterRequestLink u))
      listDefault a = case a of [] -> ""
                                (h:tail) -> h
      second z = case z of [] -> []
                           (h:tail) -> (snd h) : second tail


downloadAll :: [DownloadChapterRequest] -> IO [DownloadInfo]
downloadAll u = do
  z <- liftIO $ grabPages links
  e <- mapPages z
  let q = zip u e
  m <- Prelude.mapM (\x -> saveFiles (fst x) (downloadInfoUrl (snd x))) q
  return e
  where
    links = fmap (\x -> (pageLinkUrl (downloadChapterRequestLink x))) u
    mapPages z = Prelude.mapM htmlDownloadInfo z
    filePath u = mangaFilePath (downloadChapterRequestMangaName u) (chapterNumber u)
    saveWithPath u = saveFile (filePath u)
    saveFiles u y = Prelude.mapM (saveWithPath u) (filter (\x -> (urlValue (snd x)) /=  "http://" )  y) 
    chapterNumber u = show (downloadChapterRequestNumber u) ++ "_" ++ (pageLinkChapterName (downloadChapterRequestLink u))
    

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


groupPageDownloads :: Int -> [DownloadChapterRequest] -> [[DownloadChapterRequest]]
groupPageDownloads a [] =  []
groupPageDownloads a m = [take a m] ++ (groupPageDownloads a $ drop a m)

parseImagesFromString :: [Tag String] -> DownloadInfo
parseImagesFromString tags = do
  let x = scrapeImages
  DownloadInfo {downloadInfoUrl = scrapeImagesWithPageNumber}
  where
    s = scrape (attrs "data-src" "img") tags
    scrapeImagesWithPageNumber = ( zip [0..] scrapeImages)  
    scrapeImages = case s of Just a -> sanatizeUrl <$> a
                             Nothing -> []


htmlDownloadInfo :: T.Text -> IO DownloadInfo
htmlDownloadInfo z = do
  y <- (return $ T.unpack z)
  return (parseImagesFromString (parseTags y))

mangaFilePath :: String -> String -> Maybe (Path Rel Dir)
mangaFilePath a b = do
  x <- (parseRelDir "manga") 
  y <- (parseRelDir a)
  z <- (parseRelDir b)
  return (x </> y </> z)  


