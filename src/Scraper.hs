
module Scraper
    (
    ) where

--import Path (mkAbsDir, mkRelDir, (</>))
import Path 
import Path.Internal 
import Control.Monad.IO.Class (liftIO)

z:: Maybe (Path Rel Dir)
z = (parseRelDir "asdf") >>= (\x -> parseRelDir "asdasdff")  

cc:: String -> String -> Maybe (Path Rel Dir)
cc a b = do
  z <- (parseRelDir a) 
  y <- (parseRelDir b)
  return (z </> y)




