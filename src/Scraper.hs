{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Scraper
    (
      donTimeout,
      grabPageWithRetry,
      parseChapters,
      grabPages,
      grabPage
    ) where

import qualified Data.Text as T
import Data.List
import Path 
import Path.Internal 
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Control.Applicative
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
import ScraperData


--parseUrl :: MonadThrow m => String -> m (Url)
--parseUrl s =
--  if (=="/") s -- can use regex here
--     then return (Url s)
--     else throwM (BadUrl s)

donTimeout :: (MonadBaseControl IO m, HasCallStack) => m a -> m a -> m a
donTimeout m r = m `Control.Exception.Lifted.catch` handler
  where
    handler (SomeException e) = r 

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
    chromeConfig = useBrowser chrome defaultConfig
    --retry r = do closeSession
                   --liftIO $ grabPageWithRetry [r]

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
