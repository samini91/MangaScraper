{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE InstanceSigs #-}

module Scraper
    (
      timeout,
      grabPageWithRetry,
      grabPages,
      grabPage,
      grabPageRemoveRedundancy,
      grabPageMangaChapterLinks,
      parseAllChapters
    ) where

import qualified Data.Text as T
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Applicative
import Control.Exception.Lifted
import Test.WebDriver
import Test.WebDriver.Commands.Wait
import Text.HTML.TagSoup
import Text.HTML.Scalpel
import ScraperData
import Infra (Env(..))
import System.Log.FastLogger
import Control.Lens
import Control.Monad.IO.Class
import Utils
import Data.Text
--import Data.Either.Extra

data MangaSiteNotFoundException = MangaSiteNotFoundException deriving (Show)
instance Exception MangaSiteNotFoundException

instance ToLogStr SomeException where
  toLogStr :: SomeException -> LogStr
  toLogStr x = toLogStr $ show x

timeout :: forall a. WD a -> WD (Either SomeException a)
timeout x = do
  result <- Control.Exception.Lifted.try x :: WD (Either SomeException a)
  case result of
    Left err -> closeSession >> return (Left err)
    Right z -> return $ return z

parseAllChapters :: MangaWebSite -> [Tag String] -> [(String,String)]
-- hmm need to figure out why the compiler is complaining about matching _ _
parseAllChapters (MangaKakalot _) tags = scrapeChapters
  where
    s =  (scrape scraper tags)
    scraper = chroots (("div" @: [hasClass "chapter"]) // ("div" @: [hasClass "row"] )) $ do
      x <- (Text.HTML.Scalpel.attr "href" "a")
      y <- (text $ "a")
      return (x, y)
    scrapeChapters = case s of Just a -> a
                               Nothing -> []
parseAllChapters (MangaKatana _) tags = scrapeChapters
  where
    s =  (scrape scraper tags)
    scraper = chroots (("div" @: [hasClass "chapters"]) // ("div" @: [hasClass "chapter"] )) $ do
      x <- (Text.HTML.Scalpel.attr "href" "a")
      y <- (text $ "a")
      return (x, y)
    scrapeChapters = case s of Just a -> a
                               Nothing -> []

grabPages :: [Url] -> IO [T.Text]
grabPages x = runSession firefoxConfig $ do
  z <- Prelude.mapM openAndGetSource x
  closeSession
  return z
  where
    firefoxConfig = useBrowser firefox config
    openAndGetSource u = do openPage (urlValue u)
                            getSource
    config = defaultConfig{wdCapabilities = caps}
    caps = allCaps {additionalCaps = [("pageLoadStrategy","none")]}

grabPageWithRetry :: [Url] -> IO [T.Text]
grabPageWithRetry x =  mapRetry
  where
    mapRetry = (\y -> runSession firefoxConfig (onTimeout (grabPage y) (closeSession >> return ""))) `mapM` x -- need to take a specific amount only 
    firefoxConfig = useBrowser firefox config
    config = defaultConfig{wdCapabilities = caps}
    caps = allCaps {additionalCaps = [("pageLoadStrategy","none")]}

grabPageRemoveRedundancy :: Env -> Url ->  IO (MangaWebSite, T.Text)
grabPageRemoveRedundancy env x =
  do
    --z <- runExceptT ((grabPageWithRetryMaybe x) <|> (grabPageWithRetryMaybe x) <|> (grabPageWithRetryMaybe x))
    -- 
    z <- runExceptT ((mapErr x) <|> (mapErr x)  <|> (mapErr x))
    --z <- runExceptT (asum $ Prelude.take 3 listOfComps)
    case z of Right e -> return e
              Left err ->
                do
                  _ <- env.logFunc $ toLogStr err
                  return (getDefaultMangaWebSite, "")
  where
    listOfComps = repeat (mapErr x)
    mapErr :: Url -> ExceptT String IO (MangaWebSite, Text)
    mapErr x = withExceptT (\x -> show x) (grabPageWithRetryMaybe x)

grabPageWithRetryMaybe :: Url ->  ExceptT SomeException IO (MangaWebSite, T.Text)
grabPageWithRetryMaybe x = mapRetry
  where

    mapRetry :: ExceptT SomeException IO (MangaWebSite, T.Text)
    mapRetry = do
      m <- extract
      ExceptT $ runSession firefoxConfig $ (timeout $ logic m)

    logic y = grabPageManga y >>= (\z -> return (y,z))
    firefoxConfig = useBrowser firefox defaultConfig

    extract :: ExceptT SomeException IO MangaWebSite
    extract = ExceptT (return mangaSite)

    mangaSite :: Either SomeException MangaWebSite
    mangaSite = maybeToEither (toException MangaSiteNotFoundException) (getMangaWebSiteWithUrl x)

doMapM :: [a] -> (a -> IO (Maybe b)) -> IO (Maybe b)
doMapM x f = runMaybeT $ mapM' x f

mapM' :: [a] -> (a -> IO (Maybe b)) -> MaybeT IO b
mapM' [] _ = MaybeT $ return Nothing
mapM' (x:xs) f = MaybeT (f x) <|> mapM' xs f

grabPageMangaChapterLinks :: MangaWebSite -> IO (MangaWebSite, T.Text)
grabPageMangaChapterLinks x = siteAndData x
  where
    firefoxConfig = useBrowser firefox defaultConfig
    siteAndData :: MangaWebSite -> IO (MangaWebSite, T.Text)
    siteAndData e =
      runSession firefoxConfig $
      do
        let site = e
        pageHtml <- grabPageManga site
        return (site, pageHtml)

grabPageManga :: MangaWebSite -> WD T.Text
grabPageManga x =
  do
    setPageLoadTimeout 10000
    openPage $ urlValue $ getMangaWebSiteUrl x
    m <- getSource
    closeSession
    return m


grabPageMangaSmart :: MangaWebSite -> WD T.Text
grabPageMangaSmart x =
  do
    setPageLoadTimeout 10000
    openPage $ urlValue $ getMangaWebSiteUrl x
    m <- getSource
    closeSession
    return m


grabPage :: Url -> WD T.Text
grabPage x =
  do
    setPageLoadTimeout 10000
    openPage $ urlValue x
    m <- getSource
    closeSession
    return m
