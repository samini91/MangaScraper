{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Scraper
    (
      sanatizeUrl,
      Url(),
      urlValue,
      donTimeout,
      zc
    ) where

--import Path (mkAbsDir, mkRelDir, (</>))
import Data.Aeson
import Data.List
import GHC.Generics
import Path 
import Path.Internal 
import Control.Monad.IO.Class (liftIO)
import Network.URI.Encode
import           Control.Exception (Exception)
import           Control.Monad
import           Control.Monad.Catch (MonadThrow(..))
import Exception

import Control.Concurrent
import Control.Exception (SomeException)
import Control.Exception.Lifted
import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.CallStack
import qualified Data.Foldable as F
import Data.Text (Text)
import Data.Typeable

import Test.WebDriver.Commands
import Test.WebDriver.Class
import Test.WebDriver.Exceptions
import Test.WebDriver.Session

z:: Maybe (Path Rel Dir)
z = (parseRelDir "asdf") >>= (\x -> parseRelDir "asdasdff")  

cc:: String -> String -> Maybe (Path Rel Dir)
cc a b = do
  z <- (parseRelDir a) 
  y <- (parseRelDir b)
  return (z </> y)

a :: String -> Maybe(String)
a s = do
  return s

m = a >> a

--e = encode "\\eifjna.com"

newtype Url = Url {urlValue :: String} deriving (Show,Eq ,Generic)
instance FromJSON Url
instance ToJSON Url

data BadUrl = BadUrl String deriving (Show, Eq)
instance Exception BadUrl


-- //file-image.mpcdn.net/9482/149950/1.jpg

parseUrl :: MonadThrow m
            => String -> m (Url)

parseUrl s =
  if (=="/") s -- can use regex here
     then return (Url s)
     else throwM (BadUrl s)

sanatizeUrl :: String -> Url 
sanatizeUrl s =
  if (startsWithhttp || startsWithhttps)
  then Url sanatized
  else Url $ "http://" ++ sanatized
  where
    sanatized = dropWhile (=='/') s
    startsWithhttp = isPrefixOf "http://" s
    startsWithhttps = isPrefixOf  "https://" s
    
    

ze = Url "//asefqasdf"

--zc = sanatizeUrl "http://file-image.mpcdn.net/9482/149950/1.jpg"
zc = sanatizeUrl "http:g//"

asdf = sanatizeUrl 



donTimeout :: (MonadBaseControl IO m, HasCallStack) => m a -> m a -> m a
donTimeout m r = m `Control.Exception.Lifted.catch` handler
  where
    --handler (SomeException z) = r
    handler (SomeException e) = r 
    --handler (FailedCommand _ _) = r
    --handler (TimeoutException) = r
    
  
      --handler other = Control.Exception.Lifted.throwIO other
