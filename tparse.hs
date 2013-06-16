{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Control.Monad
import qualified Data.ByteString.Lazy as B
import qualified Data.Vector as V
import Data.HashMap.Strict ((!))
import Data.List (intercalate)
import qualified Data.Text as T
import Data.Attoparsec.Number
import Database.HDBC
import Database.HDBC.SqlValue
import Database.HDBC.PostgreSQL



parse :: B.ByteString -> Maybe Object
parse j = decode j


data W = S T.Text | F (Object -> Value)

just x = (S $ T.pack x, x)

fields :: [(W, String)]
fields = [(S"_id", "sid"), (F (const Null), "th_updated"), (F (const Null), "th_cached"),
          (F (const Null), "th_created"),
          (F (const Null), "discussion_id"), (F (const Null), "discussion_sid"),
          just "domain", just "id", just "num_comments", just "parent_id",
          (S"parent_sigid", "parent_sid"), just "points", just "text", just "title",
          (F (\o -> let String s = o!"type" in String $ T.take 1 s), "type"),
          just "url", (S"username", "user")]



insert c (Object o) = quickQuery c q fs
    where q = "insert into thrift0 (" ++ intercalate "," (map (\x -> "\""++snd x++"\"") fields) ++ ")"
              ++ "values (" ++ intercalate "," (replicate (length fields) "?") ++ ")"
          fs = map (fu . fst) fields
          fu (S s) = fu $ F (!s)
          fu (F f) | Number (I n) <- f o = toSql n
                   | String t     <- f o = toSql t
                   | Null         <- f o = SqlNull
                   | otherwise           = error $ show (f o)




insert' c (Object o) = quickQuery c "insert into thrift1 values (?,?)"
                       [toSql id, toSql $ encode o]
    where Number (I id) = o ! "id"


main = do j <- B.getContents
          c <- connectPostgreSQL "dbname=hn"
          let Just a = parse j
          let Array rs = a ! "results"
          V.forM_ rs $ \(Object r) -> insert' c (r ! "item")
          commit c

