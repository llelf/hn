module HN.NewComments.DoIt where

import HN.NewComments
import qualified HN.NewComments.DB as DB
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Time


url = "https://news.ycombinator.com/newcomments"

storeSome which resp = do now <- getCurrentTime
                          NCPage cc next <- parse now $ BC.unpack resp
                          let news = filter which cc
                          print $ length news
                          DB.storeComments news

ini = do resp <- simpleHttp url
         storeSome (const True) resp


boo = do resp <- simpleHttp url
         topId <- DB.topStored
         storeSome ((>topId) . cId) resp

