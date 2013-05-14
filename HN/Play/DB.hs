{-# LANGUAGE UnicodeSyntax #-}
module HN.Play.DB (topStored, storeComments, query, toSql) where

import Database.HDBC.PostgreSQL
import Database.HDBC
import Control.Monad
import HN.NewComments
import Data.Aeson

connection ∷ IO Connection
connection = connectPostgreSQL "dbname=hn"


query ∷ String → [SqlValue] → IO [[SqlValue]]
query q xs = connection >>= \c -> quickQuery c q xs


topStored ∷ IO Integer
topStored = do conn ← connection
               [[r]] ← quickQuery conn "select hn_id from comments1 order by hn_id desc limit 1" []
               return (fromSql r)


storeComments ∷ [Comment] → IO ()
storeComments comms = do conn ← connection
                         forM_ comms (ins conn)
                         commit conn
                         disconnect conn
    where ins conn c = run conn "insert into comments1 \
                                \ (time,\"user\",hn_id,parent,story,text) \
                                \ values (?,?,?,?,?,?)"
                       [toSql (cTime c), toSql (cUser c), toSql (cId c),
                        toSql (cParent c), toSql (cStory c),
                        toSql (commentToText c)]

              

