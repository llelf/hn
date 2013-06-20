{-# LANGUAGE UnicodeSyntax, OverloadedStrings, LambdaCase #-}

import HN.Play.DB
import Data.Aeson
--import qualified Data.ByteString.Lazy as BS
import Data.HashMap.Strict
import Data.Time.Clock.POSIX
import Data.Text (Text)
import Control.Monad
import Control.Arrow


cc ∷ Text → Text → (Value → Value) → (Text, Text, Value→Value)
cc from to fun = (from, to, fun)

ccId name = cc name name id


fromNum (Number x) = x

-- "_id": "15-f181d", => tid
-- "_update_ts": 1307463740_346270, => tupdated
-- "id": 15, => id
-- "points": 3, => score
-- "username": "sama" => user
convI = [cc "_id" "tid" id,
         cc "_update_ts" "tupdated" (Number . (/1000000) . fromNum),
         ccId "id",
         cc "points" "score" id,
         cc "username" "user" id]

-- "discussion": {
--     "id": 1, => discid
-- },
-- "parent_id": 1, => pid
-- "text": "...", => text
convC = [cc "discussion" "discid" (\case (Object d) → d!"id"
                                         x → x),
         cc "parent_id" "pid" id,
         ccId "text"]

-- ? "num_comments": 1,
-- ?"parent_id": null,
-- ?"parent_sigid": null,
-- "text": null,
-- "title": "The Hardest Lessons for Startups to Learn",
-- "url": "http://www.paulgraham.com/startuplessons.html",
convS = [ccId "text",
         ccId "title",
         ccId "url"]
                
fromX convs o = Object $ fromList [ (to, conv $ o!from) | (from,to,conv) ← convs ]

fromC = fromX $ convI ++ convC
fromS = fromX $ convI ++ convS


data Ty = S|C deriving Show

fromO (Object o) = first ($ o) from
    where from | ty == "comment"    = (fromC, C)
               | ty == "submission" = (fromS, S)
               | otherwise          = error "type"
          ty = o ! "type"




main = do c <- connection
          forM_ [0..5000199] $ \i → do
            ee <- getCT c i
            when (length ee > 0) $ store c i ee
            when (i `mod` 10000 == 0) $ print i >> commit c
          commit c
    where
      store c i [[j]] = do
        let bs = fromSql j
        let Just o = decode bs :: Maybe Value
        let o' = fromO o
        let (dat,ty) = first encode o'
        (put ty) c i dat
        return ()
      put C = putC
      put S = putS
