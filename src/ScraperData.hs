{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module ScraperData
  (
    WebSite(),
    getWebSite,
    PageNumber,
    DownloadChapterRequest(..),
    DownloadInfo(..),
    PageLinkRequest(..),
    PageLink(..),
    Url(),
    urlValue,
    sanatizeUrl
  )
where

import GHC.Generics
import Data.Aeson
import Data.List
import Data.Char

data WebSite = MangaKatana Url | MangaKakalot Url deriving (Generic, Show)
instance FromJSON WebSite
instance ToJSON WebSite

getWebSite :: String -> Maybe WebSite
getWebSite s
  | (toLowerString "MangaKatana" `isInfixOf` toLowerString s) = Just $ MangaKatana $ sanatizeUrl s
  | (toLowerString  "MangaKakalot" `isInfixOf` toLowerString s) = Just $ MangaKakalot $ sanatizeUrl s
  | otherwise = Nothing
  where
    toLowerString m = map toLower m

newtype Url = Url {urlValue :: String} deriving (Show,Eq ,Generic)
instance FromJSON Url
instance ToJSON Url

type PageNumber = Int
data DownloadChapterRequest = DownloadChapterRequest
  {
    downloadChapterRequestMangaName  :: String,
    downloadChapterRequestNumber :: Integer,
    downloadChapterRequestLink :: PageLink
  } deriving Generic
instance FromJSON DownloadChapterRequest
instance ToJSON DownloadChapterRequest

data DownloadInfo = DownloadInfo
  {
    downloadInfoUrl :: [(PageNumber,Url)]
  } deriving Generic
instance FromJSON DownloadInfo
instance ToJSON DownloadInfo


data PageLinkRequest = PageLinkRequest
  {
    --pageLinkRequestUrl :: WebSite,
    pageLinkRequestUrl :: [String],
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


sanatizeUrl :: String -> Url 
sanatizeUrl s =
  if (startsWithhttp || startsWithhttps)
  then Url sanatized
  else Url $ "http://" ++ sanatized
  where
    sanatized = dropWhile (=='/') s
    startsWithhttp = isPrefixOf "http://" s
    startsWithhttps = isPrefixOf  "https://" s
