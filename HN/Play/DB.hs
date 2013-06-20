{-# LANGUAGE UnicodeSyntax, ScopedTypeVariables #-}
module HN.Play.DB (connection, getCT,
                   putC, putS,
                   module Database.HDBC) where

import Database.HDBC.PostgreSQL
import Database.HDBC
import Control.Monad
import HN.NewComments
import Data.Aeson

connection ∷ IO Connection
connection = connectPostgreSQL "dbname=hn"


query ∷ String → [SqlValue] → IO [[SqlValue]]
query q xs = connection >>= \c -> quickQuery c q xs



getCT conn (id :: Integer) = quickQuery conn "select data from thrift1 where id=?" [toSql id]

putC c id d = quickQuery c "insert into comments values (?,?)" [toSql id, toSql d]
putS c id d = quickQuery c "insert into stories values (?,?)" [toSql id, toSql d]

