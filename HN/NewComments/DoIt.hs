module HN.NewComments.DoIt where

import HN.NewComments
import qualified HN.NewComments.DB as DB
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Time


url = "https://news.ycombinator.com/newcomments"


boo = do resp <- simpleHttp url
--         r <- responseBody resp
         now <- getCurrentTime
         NCPage cc next <- parse now $ unpack resp
         storeComments cc
--         return cc
