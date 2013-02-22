module HN.NewComments.DoIt where

import HN.NewComments
import HN.NewComments.DB
--import Network.HTTP.Conduit
import Network.HTTP
import Data.Time



url = "http://news.ycombinator.com/newcomments"


boo = do resp <- simpleHTTP (getRequest url)
         r <- getResponseBody resp
         now <- getCurrentTime
         NCPage cc next <- parse now r
--         storeComments cc
         return cc


