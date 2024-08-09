{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
module ScraperData
  (
    MangaWebSite(),
    getMangaWebSite,
    getMangaWebSiteUrl,
    getMangaWebSiteWithUrl,
    getMangaWebSiteWithUrlList,
    getDefaultMangaWebSite,
    PageNumber,
    DownloadChapterRequest(..),
    DownloadInfo(..),
    PageLinkRequest(..),
    PageLink(..),
    Url(),
    urlValue,
    sanatizeUrl,
    pattern MangaKatana,
    pattern MangaKakalot
  )
where

import GHC.Generics
import Data.Aeson
import Data.List
import Data.Char
import Data.Maybe


data MangaWebSite = MangaKatana_ Url | MangaKakalot_ Url deriving (Generic, Show)
--newtype MangaWebSite s = MangaWebSite Url deriving (Generic, Show)
instance FromJSON MangaWebSite
instance ToJSON MangaWebSite

pattern MangaKatana a <- MangaKatana_ a
pattern MangaKakalot a <- MangaKakalot_ a
--data ActualSites =  MangaKatana Url | MangaKakalot Url deriving (Generic, Show)

getDefaultMangaWebSite :: MangaWebSite
getDefaultMangaWebSite = MangaKatana_ $ sanatizeUrl ""

getMangaWebSiteUrl :: MangaWebSite -> Url -- there is probably a bhttps://www.google.com/search?client=ubuntu&channel=fs&q=haskell+regex+replace&ie=utf-8&oe=utf-8etter way to do this
getMangaWebSiteUrl (MangaKatana a) = a
getMangaWebSiteUrl (MangaKakalot a) = a

getMangaWebSite :: String -> Maybe MangaWebSite
getMangaWebSite s
  | toLowerString "MangaKatana" `isInfixOf` toLowerString s = Just $ MangaKatana_ $ sanatizeUrl s
  | toLowerString  "MangaKakalot" `isInfixOf` toLowerString s = Just $ MangaKakalot_ $ sanatizeUrl s
  | otherwise = Nothing
  where
    toLowerString m = map toLower m

getMangaWebSiteWithUrl :: Url -> Maybe MangaWebSite
getMangaWebSiteWithUrl s
  | toLowerString "MangaKatana" `isInfixOf` urlValue s = Just $ MangaKatana_ $  s
  | toLowerString  "MangaKakalot" `isInfixOf` urlValue s = Just $ MangaKakalot_ $ s
  | otherwise = Nothing
  where
    toLowerString = map toLower

getMangaWebSiteWithUrlList :: [Url] -> [MangaWebSite]
getMangaWebSiteWithUrlList [] = []
getMangaWebSiteWithUrlList (x:xs) = maybeToList (getMangaWebSiteWithUrl x) ++ (getMangaWebSiteWithUrlList $ xs)

newtype Url = Url {urlValue :: String} deriving (Show,Eq ,Generic)
instance FromJSON Url
instance ToJSON Url

type PageNumber = String
--type PageNumber = Int
data DownloadChapterRequest = DownloadChapterRequest -- we create this server side
  {
    downloadChapterRequestMangaName  :: String,
    downloadChapterRequestNumber :: Int,
    downloadChapterRequestLink :: PageLink
  } deriving Generic
instance FromJSON DownloadChapterRequest
instance ToJSON DownloadChapterRequest

data DownloadInfo = DownloadInfo
  {
    downloadInfoUrl :: [(PageNumber,Url)] -- these are plain old urls that we are going to map download
  } deriving Generic
instance FromJSON DownloadInfo
instance ToJSON DownloadInfo

data PageLinkRequest = PageLinkRequest -- external api 
  {
    pageLinkRequestUrl :: String, -- convert into mangawebsite
    pageLinkRequestMangaName :: String
  } deriving (Generic,Show)
instance FromJSON PageLinkRequest
instance ToJSON PageLinkRequest

data PageLink = PageLink {
  pageLinkMangaName :: String,
  --pageLinkUrl:: Url ,
  pageLinkUrl:: MangaWebSite , -- here we are looking directly at the type of the url to figure out how to parse it
  pageLinkChapterName::String
  } deriving Generic
instance FromJSON PageLink
instance ToJSON PageLink


sanatizeUrl :: String -> Url
sanatizeUrl s =
  if startsWithhttp || startsWithhttps
  then Url sanatized
  else Url $ "http://" ++ sanatized
  where
    sanatized = dropWhile (=='/') s
    startsWithhttp = "http://" `isPrefixOf` s
    startsWithhttps = "https://" `isPrefixOf` s
