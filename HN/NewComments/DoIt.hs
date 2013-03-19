module HN.NewComments.DoIt where

import HN.NewComments
import qualified HN.NewComments.DB as DB
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Time


url = "https://news.ycombinator.com/newcomments"

ini = do resp <- simpleHttp url
         now <- getCurrentTime
         NCPage cc next <- parse now $ BC.unpack resp
         DB.storeComments cc


boo = do resp <- simpleHttp url
         now <- getCurrentTime
         NCPage cc next <- parse now $ BC.unpack resp
         topId <- DB.topStored
         let news = filter ((>topId) . cId) cc
         print $ length news
         DB.storeComments news


