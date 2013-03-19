{-# LANGUAGE UnicodeSyntax #-}
module HN.NewComments.DB (topStored, storeComments) where

import Database.HDBC.PostgreSQL
import Database.HDBC
import Control.Monad
import HN.NewComments
import Data.Aeson

connection ∷ IO Connection
connection = connectPostgreSQL "dbname=hn"


topStored ∷ IO Integer
topStored = do conn ← connection
               [[r]] ← quickQuery conn "select id from comments order by id limit 1" []
               return (fromSql r)


storeComments ∷ [Comment] → IO ()
storeComments comms = do conn ← connection
                         forM_ comms (ins conn)
                         commit conn
                         disconnect conn
    where ins conn c = run conn "insert into comments (id,data) values (?,?)"
                       [toSql (cId c), toSql (encode c)]

              

