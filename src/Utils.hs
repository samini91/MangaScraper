module Utils
  (
    mapConcurrentlyWithLimit
  )
  where
import Control.Monad.IO.Class (MonadIO)
import Control.Concurrent.Async (mapConcurrently_, mapConcurrently)

type Limit = Int
mapConcurrentlyWithLimit ::  (a -> IO b) -> [a] -> Limit -> IO [b]
mapConcurrentlyWithLimit fn list limit =
  do
    fnItems <- mapConcurrently fn (take limit list)
    rest <- mapConcurrentlyWithLimit fn (drop limit list) limit
    return (fnItems ++ rest)





