{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scraper
    (
      timeoutMaybe,
      grabPageWithRetry,
      --parseChapters,
      grabPages,
      grabPage,
      grabPageRemoveRedundancy,
      grabPageMangaChapterLinks,
      parseAllChapters
      --tempParseChapters
    ) where

import qualified Data.Text as T
import Data.List
import Data.Typeable
import Path 
import Path.Internal 
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad
import Data.Maybe
import Control.Monad.Catch
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe 
import Control.Applicative
import Control.Concurrent
import Control.Exception (tryJust, try, SomeException)
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
import ScraperData

timeoutMaybe :: forall a. WD a -> WD (Maybe a)
timeoutMaybe x = do
  result <- (Control.Exception.Lifted.try x) :: WD (Either SomeException a)
  case result of
    Left ex -> closeSession >> return Nothing
    Right z -> return $ Just (z)

--parseChapters :: (MangaWebSite, [Tag String]) -> [(String,String)]
--parseChapters (MangaKatana e) tags = scrapeChapters
-- parseChapters :: [Tag String] -> [(String,String)]
-- parseChapters tags = scrapeChapters
--   where
--     s =  (scrape scraper tags)
--     scraper = chroots(("div" @: [hasClass "chapters"]) // ("div" @: [hasClass "chapter"] )) $ do
--       x <- (Text.HTML.Scalpel.attr "href" "a") 
--       y <- (text $ "a")
--       return (x, y)
--     scrapeChapters = case s of Just a -> a
--                                Nothing -> []

parseAllChapters :: MangaWebSite -> [Tag String] -> [(String,String)]
parseAllChapters (MangaKakalot _) tags = scrapeChapters
  where
    s =  (scrape scraper tags)
    scraper = chroots(("div" @: [hasClass "chapter"]) // ("div" @: [hasClass "row"] )) $ do
      x <- (Text.HTML.Scalpel.attr "href" "a") 
      y <- (text $ "a")
      return (x, y)
    scrapeChapters = case s of Just a -> a
                               Nothing -> []
parseAllChapters (MangaKatana _) tags = scrapeChapters
  where
    s =  (scrape scraper tags)
    scraper = chroots(("div" @: [hasClass "chapters"]) // ("div" @: [hasClass "chapter"] )) $ do
      x <- (Text.HTML.Scalpel.attr "href" "a") 
      y <- (text $ "a")
      return (x, y)
    scrapeChapters = case s of Just a -> a
                               Nothing -> []
                               
                               

grabPages :: [Url] -> IO [T.Text]
grabPages x = runSession chromeConfig $ do
  z <- Prelude.mapM openAndGetSource x
  closeSession                         
  return z
  where
    chromeConfig = useBrowser chrome defaultConfig
    openAndGetSource x = do openPage (urlValue x)
                            getSource

grabPageWithRetry :: [Url] -> IO [T.Text]
grabPageWithRetry x =  mapRetry
  where
    mapRetry = (\y -> runSession chromeConfig (onTimeout(grabPage y)(closeSession >> return ""))) `mapM` x -- need to take a specific amount only 
    --chromeConfig = useBrowser chrome defaultConfig
    chromeConfig = useBrowser chrome config
    config = defaultConfig{wdCapabilities = caps}
    caps = allCaps {additionalCaps = [("pageLoadStrategy","none")]}
    --retry r = do closeSession
                   --liftIO $ grabPageWithRetry [r]

grabPageWithRetry' :: [Url] -> IO [(MangaWebSite, T.Text)]
grabPageWithRetry' x = mapRetry
  where
    mapRetry = (\y -> runSession chromeConfig (onTimeout(grabPageManga y >>= (\z -> return (y,z)))(closeSession >> return (getDefaultMangaWebSite, "")))) `mapM` mangaSites -- onTimeout calls for a wd not a maybeT WD...```
    --chromeConfig = useBrowser chrome defaultConfig
    chromeConfig = useBrowser chrome config
    config = defaultConfig{wdCapabilities = caps}
    caps = allCaps {additionalCaps = [("pageLoadStrategy","none")]}
    mangaSites = getMangaWebSiteWithUrlList x

grabPageRemoveRedundancy :: [Url] ->  IO (MangaWebSite, T.Text)
grabPageRemoveRedundancy x =
  do
    z <- grabPageWithRetryMaybe x
    case z of Just e -> return e
              Nothing -> return (getDefaultMangaWebSite, "")
  
--grabpagewithretrymaybe :: [Url] ->  IO [Maybe (MangaWebSite, T.Text)]
grabPageWithRetryMaybe :: [Url] ->  IO (Maybe (MangaWebSite, T.Text) )
--grabPageWithRetryMaybe :: [Url] ->  IO  (MangaWebSite, T.Text) 
--grabPageWithRetryMaybe :: [Url] ->  IO [(MangaWebSite, T.Text)]
grabPageWithRetryMaybe x = mapRetry
--grabPageWithRetryMaybe x = 
  where
    --mapRetry = (\y -> runSession chromeConfig (return $ (liftIO $ timeoutMaybe (logic y))  )) `mapM` mangaSites
    --mapRetry = (\y -> runSession chromeConfig (runMaybeT $ timeoutMaybe (logic y)) ) `mapM` mangaSites
    -- mapRetry = (\y ->  runSession chromeConfig (timeoutMaybe (logic y)) ) `mapM` mangaSites good
    mapRetry = doMapM mangaSites (\y ->  runSession chromeConfig (timeoutMaybe (logic y)) )  
    --mapRetry = msum (\y -> runSession chromeConfig (timeoutMaybe (logic y)) )  mangaSites
    --mapRetry = mfold (\y -> runSession chromeConfig (timeoutMaybe (logic y)) )  mangaSites
    logic y = grabPageManga y >>= (\z -> return $ (y,z))
    --z y = tryit (liftIO (logic y))
    chromeConfig = useBrowser chrome defaultConfig
    config = defaultConfig{wdCapabilities = caps}
    caps = allCaps {additionalCaps = [("pageLoadStrategy","none")]}
    mangaSites = getMangaWebSiteWithUrlList x


doMapM :: [a] -> (a -> IO (Maybe b)) -> IO (Maybe b)
doMapM x f = runMaybeT $ mapM' x f 

mapM' :: [a] -> (a -> IO (Maybe b)) -> MaybeT IO b
mapM' [] f = MaybeT $ return Nothing
mapM' (x:xs) f = MaybeT (f x) <|> mapM' xs f

grabPageMangaChapterLinks :: [MangaWebSite] -> IO [(MangaWebSite, T.Text)]
--grabPageMangaChapterLinks x = runSession chromeConfig $ siteAndData `mapM` x
grabPageMangaChapterLinks x = siteAndData `mapM` x 
  where
    chromeConfig = useBrowser chrome defaultConfig
    siteAndData :: MangaWebSite -> IO (MangaWebSite, T.Text)
    siteAndData e =
      runSession chromeConfig $
      do
        site <- return e
        pageHtml <- grabPageManga site
        return (site, pageHtml)

grabPageManga :: MangaWebSite -> WD (T.Text)
grabPageManga x =
  do
    setPageLoadTimeout 10000 
    openPage $ urlValue $ getMangaWebSiteUrl x
    m <- getSource
    closeSession
    return m
  where
    --chromeConfig = useBrowser chrome config
    chromeConfig = useBrowser chrome defaultConfig
    config = defaultConfig{wdCapabilities = caps}
    caps = allCaps {additionalCaps = [("pageLoadStrategy","none")]}
    defaultval a = case a of Just a -> a
                             Nothing -> sanatizeUrl "" 
    --caps = allCaps {additionalCaps = [("pageLoadStrategy","eager")]}


--grabPage' :: Url -> MaybeT WD (MangaWebSite ,T.Text) -- Monad Transformer here
grabPage' :: Url -> MaybeT WD (T.Text) 
--grabPage' :: Url -> WD (Maybe (MangaWebSite ,T.Text)) -- Monad Transformer here
grabPage' x =
  do
    setPageLoadTimeout 10000 
    z <- MaybeT $ return $ getMangaWebSiteWithUrl x
    openPage $ urlValue $ getMangaWebSiteUrl z
    m <- getSource
    closeSession
    return m
    --return Just(,m)
  where
    chromeConfig = useBrowser chrome config
    config = defaultConfig{wdCapabilities = caps}
    caps = allCaps {additionalCaps = [("pageLoadStrategy","none")]}
    defaultval a = case a of Just a -> a
                             Nothing -> sanatizeUrl "" 
    --caps = allCaps {additionalCaps = [("pageLoadStrategy","eager")]}

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
    caps = allCaps {additionalCaps = [("pageLoadStrategy","none")]}
    --caps = allCaps {additionalCaps = [("pageLoadStrategy","eager")]}
