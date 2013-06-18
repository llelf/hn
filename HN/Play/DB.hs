{-# LANGUAGE UnicodeSyntax #-}
module HN.Play.DB (toSql) where

import Database.HDBC.PostgreSQL
import Database.HDBC
import Control.Monad
import HN.NewComments
import Data.Aeson

connection ∷ IO Connection
connection = connectPostgreSQL "dbname=hn"


query ∷ String → [SqlValue] → IO [[SqlValue]]
query q xs = connection >>= \c -> quickQuery c q xs



