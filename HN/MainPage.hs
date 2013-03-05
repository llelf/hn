{-# LANGUAGE Arrows #-}
module HN.MainPage where

import Text.XML.HXT.Core
import Text.XML.HXT.TagSoup
import HN.Parsing

data SParse = Title String String | Info String deriving Show

doc' = readDocument opts "../newest.html"
opts = [ withWarnings False, withParseHTML True, withTagSoup ]

nn = doc' /> hasName "html" /> hasName "body" /> hasName "center" /> hasName "table"
     /> hasName "tr" /> hasName "td" /> hasName "table" /> hasName "tr" >>> title <+> info
    where td = getChildren >>> hasName "td"
          title = proc x ->
                  do a <- td >>> hasAttrValue "class" (=="title") /> hasName "a" -< x
                     link <- getAttrValue "href" -< a
                     title <- getChildren >>> getText -< a
                     returnA -< Title link title
          info  = proc x ->
                  do sub <- td >>> hasAttrValue "class" (=="subtext") -< x
                     user <- getChildren >>> aUser -< sub
                     returnA -< Info user



