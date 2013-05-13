{-# LANGUAGE UnicodeSyntax #-}
module HN.Parsing (aUser, xpath, aItemId, aHrefPrefix, opts,
                   module Text.XML.HXT.Core,
                   module Control.Arrow.Unicode)
    where

import Text.XML.HXT.Core
import Text.XML.HXT.XPath
import Text.XML.HXT.TagSoup
import Control.Arrow.Unicode
import Data.List
import Data.String


opts = [ withWarnings False, withParseHTML True, withTagSoup ]


xpath ∷ ArrowXml cat ⇒ String → cat XmlTree XmlTree
xpath = getXPathTrees


aUser ∷ ArrowXml cat ⇒ cat XmlTree String
aUser = aHrefPrefix "user" /> getText

aItemId inside = aHrefPrefix "item" >>> getId inside
aHrefPrefix x = hasName "a" >>> hasAttrValue "href" (x `isPrefixOf`)


--getId ∷ ArrowXml cat ⇒ cat XmlTree ID
getId f = (getChildren >>> hasText f) `guards` getAttrValue "href" >>> arr (maybe 0 read . stripItem)
stripItem = stripPrefix ("item?id=" ∷ String)

