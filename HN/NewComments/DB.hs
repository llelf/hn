
module HN.NewComments.DB where

import Database.HDBC.PostgreSQL
import Database.HDBC
import Control.Monad
import HN.NewComments

topStored :: Connection -> IO Integer
topStored conn = do [[r]] <- quickQuery conn "select id from comments order by id limit 1" []
                    return (fromSql r)

storeComments comms = do conn <- connectPostgreSQL "dbname=hn"
                         forM comms (ins conn)
                         commit conn
                         disconnect conn
    where ins conn c = run conn "insert into comments (id,\"user\",parent,story,time,text) values (?,?,?,?,?,?)"
                       [toSql (cId c), toSql (cUser c), toSql (cParent c), toSql (cStory c),
                        toSql (cTime c), toSql (cText c)]
              

