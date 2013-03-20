module HN.NewComments.DoIt where

import HN.NewComments
import qualified HN.NewComments.DB as DB
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Time
import Data.Time.Format
import System.Locale

url = "https://news.ycombinator.com/newcomments"

syslog s = do t <- getCurrentTime
              appendFile "log" (formatTime defaultTimeLocale "%c" t ++ " " ++ s ++ "\n")


storeSome which resp = do now <- getCurrentTime
                          NCPage cc next <- parse now $ BC.unpack resp
                          let news = reverse . filter which $ cc
                          print $ length news
                          syslog $ "new " ++ show (length news)
                          DB.storeComments news

ini = do resp <- simpleHttp url
         storeSome (const True) resp


boo = do resp <- simpleHttp url
         topId <- DB.topStored
         storeSome ((>topId) . cId) resp

