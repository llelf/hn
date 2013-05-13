{-# LANGUAGE Arrows, NoMonomorphismRestriction, TupleSections, UnicodeSyntax,
  OverloadedStrings #-}

module HN.NewComments (NCPage(..),
                       runX, parse)
    where

-- html>body>center>table> tr>td>table>tr> td[class=default]

import Prelude hiding (span)
import Prelude.Unicode
import Control.Arrow.Unicode
import Control.Applicative
import Text.XML.HXT.Core
import Text.HandsomeSoup
import Data.List hiding (span)
import Data.List.Split
import Control.Monad (join)
import Data.Aeson
import Data.Time
import Text.Pandoc

import HN.Parsing
import HN.Comment


doc = readDocument opts "nc-fmt.html"


data NCPage = NCPage [Comment] String deriving Show

s1 = "Garageband is the app"
s2 = "Reminds me of the Joel"

showSome substr (NCPage pagecs _) = do putStrLn $ show (length cs) ++ " comments"
                                       print $ cText $ head cs
                                       putStrLn $ commentToText $ head cs
    where cs = filter ((substr `isInfixOf`) . commentToText) $ pagecs

-- sh' file s = do p <- pp file
--                showSome s p
sh file s = join $ showSome s <$> pp file



parse :: UTCTime -> String -> IO NCPage
parse now s = do let doc = readString opts s
                 comms <- runX $ parseComment now doc
                 --[nxt] <- runX $ next doc
                 return $ NCPage comms ""

pp file = do f <- readFile file
             t <- getCurrentTime
             p <- parse t f
             return p




next doc = doc //> hasName "td" >>> hasAttrValue "class" (=="title")
           /> aHrefPrefix "/x?fnid" >>> getAttrValue "href"



