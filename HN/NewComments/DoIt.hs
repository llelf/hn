module HN.NewComments.DoIt where

import HN.NewComments
import HN.NewComments.DB
import Network.HTTP.Conduit
import Data.ByteString.Lazy.Char8
import Data.Time


url = "https://news.ycombinator.com/newcomments"


boo = do resp <- simpleHttp url
--         r <- responseBody resp
         now <- getCurrentTime
         NCPage cc next <- parse now $ unpack resp
         storeComments cc
--         return cc
