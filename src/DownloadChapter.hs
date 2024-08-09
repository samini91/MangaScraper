{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

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
import Control.Monad.Trans.Except (ExceptT)
import Network.HTTP.Client (HttpException (HttpExceptionRequest))
import Control.Exception.Lifted
import System.Log.FastLogger
import Infra (Env(..))

download :: Env -> DownloadChapterRequest -> IO DownloadInfo
download env u = do
  m <- grabPageRemoveRedundancy $ [getMangaWebSiteUrl (link u)]
  let log = getMangaWebSiteUrl (link u)
  _ <- env.logFunc $ toLogStr (show log)
  let z = snd m
  let strHtml = T.unpack z
  let x = parseImages (fst m) (parseTags strHtml)
  res <- saveFiles (downloadInfoUrl x)
  return (DownloadInfo { downloadInfoUrl = x.downloadInfoUrl })
  where
      filePath = mangaFilePath (downloadChapterRequestMangaName u) chapterNumber
      saveWithPath y = catchHttpException $ saveFile filePath y
      saveFiles y = mapConcurrently saveWithPath (filter (\x -> urlValue (snd x) /= "http://") y)
      chapterNumber = (addPadding 4 (downloadChapterRequestNumber u)) ++ "_" ++ pageLinkChapterName (downloadChapterRequestLink u)
      link r = pageLinkUrl $ downloadChapterRequestLink r

catchHttpException:: forall a . IO a -> IO (Either HttpException a) -- pass in the path instead
catchHttpException x = Control.Exception.Lifted.try x  :: IO (Either HttpException a)

saveFile :: Maybe(Path Rel Dir)-> (PageNumber, Url) -> IO FilePath -- pass in the path instead 
saveFile p x = do
  m <- get (urlValue $ snd x)
  let z = m ^. responseBody
  createDirectoryIfMissing True filePath -- get string representation here return FilePath of "" if maybe is empty
  B.writeFile fileName z
  return filePath
  where
    filePath = case (fromRelDir <$> p) of
                 Nothing -> ""
                 Just a -> a
    fileName = filePath ++ (fst x) ++ ".jpg" -- should wrap this in a monad too

-- katana
parseImagesKatana :: [Tag String] -> DownloadInfo
parseImagesKatana tags =
  DownloadInfo {downloadInfoUrl = scrapeImagesWithPageNumber}
  where
    s = scrape (attrs "data-src" "img") tags
    scrapeImagesWithPageNumber = zip ((addPadding 4) <$> [0..]) scrapeImages
    scrapeImages = case s of Just a -> sanatizeUrl <$> a
                             Nothing -> []

duplicate :: String -> Int -> String
duplicate string n = concat $ replicate n string


-- possible padding is less than len I guess we should do nothing in that case
addPadding :: Int -> Int -> String
addPadding paddingAmt i =
  let
    s = show i
    len = length s
   in
    duplicate "0" (paddingAmt - len) ++ s


-- kakalot
parseImagesKakalot :: [Tag String] -> DownloadInfo
parseImagesKakalot tags =
  DownloadInfo {downloadInfoUrl = scrapeImagesWithPageNumber}
  where
    s = scrape (attrs "src" "img") tags
    scrapeImagesWithPageNumber = zip ((addPadding 4) <$> [0..]) scrapeImages
--    scrapeImagesWithPageNumber = zip [0..] scrapeImages
    scrapeImages = case s of Just a -> sanatizeUrl <$> a
                             Nothing -> []


parseImages :: MangaWebSite -> [Tag String] -> DownloadInfo
parseImages (MangaKatana _) l = parseImagesKatana l
parseImages (MangaKakalot _) l = parseImagesKakalot l

mangaFilePath :: String -> String -> Maybe (Path Rel Dir)
mangaFilePath a b = do
  _0 <- (parseRelDir "manga")
  _1 <- (parseRelDir a)
  _2 <- (parseRelDir b)
  return (_0 </> _1 </> _2)
  





--mangaFilePath :: String -> String -> String -> Maybe (Path Rel Dir)
--mangaFilePath a b c = do
--  _0 <- (parseRelDir "manga")
--  _1 <- (parseRelDir a)
--  _2 <- (parseRelDir b)
--  _3 <- (parseRelDir c)
--  return (_0 </> _1 </> _2 </> _3)
