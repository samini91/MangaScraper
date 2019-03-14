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

import Data.Aeson
import qualified Data.Text as T
import Data.List (intercalate)
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import GHC.Generics
import Network.HTTP
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
import Control.Exception (Exception, catch, SomeException)
import Path 
import Path.Internal
import Scraper

type PageNumber = Int
data DownloadChapterRequest = DownloadChapterRequest
  {
    --downloadChapterRequestUrl :: String ,
    downloadChapterRequestMangaName  :: String,
    downloadChapterRequestNumber :: Integer,
    downloadChapterRequestLink :: PageLink
  } deriving Generic
instance FromJSON DownloadChapterRequest
instance ToJSON DownloadChapterRequest

data DownloadInfo = DownloadInfo
  {
    downloadInfoUrlDump :: String,
    downloadInfoUrl :: [(PageNumber,Url)]
  } deriving Generic
instance FromJSON DownloadInfo
instance ToJSON DownloadInfo

data PageLinkRequest = PageLinkRequest
  {
    pageLinkRequestUrl :: String,
    pageLinkRequestMangaName :: String
  } deriving (Generic,Show)
instance FromJSON PageLinkRequest
instance ToJSON PageLinkRequest

data PageLink = PageLink {
  pageLinkMangaName :: String,
  pageLinkUrl:: Url ,
  pageLinkChapterName::String
  } deriving Generic

instance FromJSON PageLink
instance ToJSON PageLink

type API = "downloadManga" :> ReqBody '[JSON] PageLinkRequest :> Post '[JSON] [PageLink]
           
startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Servant.Proxy API
api = Servant.Proxy

server :: Server API
server = downloadMangaHandler

downloadMangaHandler :: PageLinkRequest -> Handler [PageLink]
downloadMangaHandler r = liftIO $ do
  downloadManga r

downloadManga :: PageLinkRequest -> IO [PageLink]
downloadManga r = do
  x <- (grabLinks r)
  l <- return ( (reverse x))
  --l <- return ( take 1 $ (reverse x))
  let chapters = zip [0..] l
  m <- return (map (\z -> DownloadChapterRequest{
                       downloadChapterRequestMangaName = pageLinkRequestMangaName r,
                       downloadChapterRequestNumber = (fst z) ,
                       downloadChapterRequestLink = (snd z)}) chapters)
  --downloadAll m
  downloadAllm m
  return l
  where
    downloadAllm x = mapM download x
    --downloadAllm x = mapConcurrently download x -- this uses too much memory 

grabLinksHandler :: PageLinkRequest -> Handler [PageLink]
grabLinksHandler r = liftIO $ grabLinks r

grabLinks :: PageLinkRequest -> IO [PageLink]
grabLinks r = do
  z <- grabPageWithRetry (sanatizeUrl $ pageLinkRequestUrl r)
  --z <- test (sanatizeUrl $ pageLinkRequestUrl r)
  let strHtml = T.unpack z
  m <- return (parseChapters (parseTags strHtml))
  return (map (\x -> PageLink{ pageLinkUrl = (sanatizeUrl (fst x)) , pageLinkChapterName = (snd x), pageLinkMangaName = pageLinkRequestMangaName r}) m)

mangaFilePath :: String -> String -> Maybe (Path Rel Dir)
mangaFilePath a b = do
  x <- (parseRelDir "manga") 
  y <- (parseRelDir a)
  z <- (parseRelDir b)
  return (x </> y </> z)  

download :: DownloadChapterRequest -> IO DownloadInfo
download u = do
  z <- grabPageWithRetry link
  let strHtml = T.unpack z
  x <-  return (parseImagesFromString (parseTags strHtml))
  forkIO ((\x -> ()) <$> (saveFiles (downloadInfoUrl x)))
  --(saveFiles (downloadInfoUrl x))
  return (DownloadInfo {downloadInfoUrlDump = "", downloadInfoUrl = (downloadInfoUrl x)})
  where
      filePath = mangaFilePath (downloadChapterRequestMangaName u) chapterNumber 
      saveWithPath = saveFile (filePath)
      saveFiles y = mapM saveWithPath (filter (\x -> urlValue (snd x) /= "http://")  y) 
      chapterNumber = show (downloadChapterRequestNumber u) ++ "_" ++ (pageLinkChapterName (downloadChapterRequestLink u))
      link = (pageLinkUrl (downloadChapterRequestLink u))
      second z = case z of [] -> []
                           (h:tail) -> (snd h) : second tail


downloadAll :: [DownloadChapterRequest] -> IO [DownloadInfo]
downloadAll u = do
  z <- liftIO $ grabPages links
  e <- mapPages z
  let q = zip u e
  m <- mapM (\x -> saveFiles (fst x) (downloadInfoUrl (snd x))) q
  return e
  where
    links = fmap (\x -> (pageLinkUrl (downloadChapterRequestLink x))) u
    mapPages z = mapM htmlDownloadInfo z
    filePath u = mangaFilePath (downloadChapterRequestMangaName u) (chapterNumber u)
    saveWithPath u = saveFile (filePath u)
    saveFiles u y = mapM (saveWithPath u) (filter (\x -> (urlValue (snd x)) /=  "http://" )  y) 
    chapterNumber u = show (downloadChapterRequestNumber u) ++ "_" ++ (pageLinkChapterName (downloadChapterRequestLink u))
    

htmlDownloadInfo :: T.Text -> IO DownloadInfo
htmlDownloadInfo z = do
  y <- (return $ T.unpack z)
  z <- return (parseImagesFromString (parseTags y))
  --(saveFiles (downloadInfoUrl z))
  return z

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

-------------- Download Page ------------

parseChapters :: [Tag String] -> [(String,String)]
parseChapters tags = scrapeChapters
  where
    s =  (scrape scraper tags)
    scraper = chroots(("div" @: [hasClass "chapters"]) // ("div" @: [hasClass "chapter"] )) $ do
      x <- (Text.HTML.Scalpel.attr "href" "a") 
      y <- (text $ "a")
      return (x, y)
    scrapeChapters = case s of Just a -> a
                               Nothing -> []

parseImagesFromString :: [Tag String] -> DownloadInfo
parseImagesFromString tags = do
  let x = scrapeImages
  --DownloadInfo {downloadInfoUrlDump = (Data.List.intercalate "   " (urlValue x) ), downloadInfoUrl = scrapeImagesWithPageNumber}
  DownloadInfo {downloadInfoUrlDump = "", downloadInfoUrl = scrapeImagesWithPageNumber}
  where
    s = scrape (attrs "data-src" "img") tags
    scrapeImagesWithPageNumber = ( zip [0..] scrapeImages)  
    scrapeImages = case s of Just a -> sanatizeUrl <$> a
                             Nothing -> []


grabPages :: [Url] -> IO [T.Text]
grabPages x = runSession chromeConfig $ do
  z <- mapM openAndGetSource x
  closeSession                         
  return z
  where
    chromeConfig = useBrowser chrome defaultConfig
    openAndGetSource x = do openPage (urlValue x)
                            getSource
    
grabPageWithRetry :: Url -> IO T.Text
grabPageWithRetry x = runSession chromeConfig $
  do
    onTimeout (grabPage x)(liftIO $ grabPageWithRetry x)
  where
    chromeConfig = useBrowser chrome defaultConfig

grabPage :: Url -> WD T.Text
grabPage x =
  do
    setPageLoadTimeout 10000 
    openPage $ urlValue x
    m <- getSource
    closeSession
    return m
  where
    chromeConfig = useBrowser chrome config
    config = defaultConfig{wdCapabilities = caps}
    --caps = allCaps {additionalCaps = [("pageLoadStrategy","none")]}
    caps = allCaps {additionalCaps = [("pageLoadStrategy","eager")]}
  
