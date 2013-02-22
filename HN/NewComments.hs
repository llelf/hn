{-# LANGUAGE Arrows, NoMonomorphismRestriction, UnicodeSyntax #-}

module HN.NewComments (NCPage(..),Comment(..),runX,parse) where

-- html>body>center>table> tr>td>table>tr> td[class=default]

import Prelude hiding (span)
import Control.Applicative
import Text.XML.HXT.Core
import Text.XML.HXT.TagSoup
import Text.XML.HXT.XPath
import Text.HandsomeSoup
--import Text.XML.HXT.DOM.FormatXmlTree
import Data.List hiding (span)
import Control.Monad
import Data.Time

--import Text.Pandoc.Definition

xpath = getXPathTrees

doc = readDocument opts "nc-fmt.html"
doc' = readDocument opts "../newest.html"


opts = [ withWarnings False, withParseHTML True, withTagSoup ]

data NCPage = NCPage [Comment] String deriving Show

s1 = "Garageband is the app"
s2 = "Reminds me of the Joel"

showSome substr (NCPage pagecs _) = do putStrLn $ show (length cs) ++ " comments"
                                       putStrLn $ cText $ head cs
    where cs = filter ((substr `isInfixOf`) . cText) pagecs

-- sh' file s = do p <- pp file
--                showSome s p
sh file s = join $ showSome s <$> pp file



parse :: UTCTime -> String -> IO NCPage
parse now s = do let doc = readString opts s
                 comms <- runX $ cc now doc
                 --[nxt] <- runX $ next doc
                 return $ NCPage comms ""

pp file = do f <- readFile file
             t <- getCurrentTime
             p <- parse t f
             return p



pComment = getChildren >>> hasName "font" /> getText

type ID = Int

data Voted = Normal | Downvoted | Dead
             deriving Show

data AgoUnit = AgoM | AgoH | AgoD

data Comment = Comment { cUser :: String, cId :: ID,
                         cParent :: ID, cStory :: ID, cText :: String,
                         cTime :: UTCTime,
                         cVoted :: Voted }
               deriving Show

-- instance Show Comment where
--     show c = (cUser c) ++ ":<" ++ take 15 (head $ cText c) ++ "..>"


-- X minute(s) ago, X hour(s) ago, X day(s) ago
parseAgo s now | (sn : q : "ago" : _) <- words s = addUTCTime (negate $ fromIntegral (read sn) * qToSecs q) now
               | otherwise                       = now
               where
                 qToSecs x | "minute" `isPrefixOf` x = 60
                           | "hour" `isPrefixOf` x   = 60 * 60
                           | "day" `isPrefixOf` x    = 60 * 60 * 24


-- span>font>p>a
commentText :: ArrowXml cat => cat XmlTree [String]
commentText = listA $ getChildren >>> this <+> getChildren
              >>> getXPathTrees "span/font" /> par
    where par = ((getText
                  <+> (hasName "a" >>> getAttrValue "href")
                  <+> (hasName "i" /> getText >>> arr ((++"*").("*"++))))
                 `orElse` (arr (("MOOOOOO:"++) . show))
                )
                -- `orElse` (proc x -> do b <- getName -< x
                --                        returnA -< "###<" ++ b ++ ">")

          par' = proc x -> do t <- getText -< x
                              n <- getName -< x
                              returnA -< "C[" ++ t ++ "] N[" ++ n ++ "]"
                             

cc ∷ ArrowXml cat ⇒ UTCTime → cat a XmlTree → cat a Comment
cc now doc =
    doc >>> getXPathTrees "//html/body/center/table/tr/td/table/tr/td[@class='default']"
        >>> proc x ->
             do user ← span /> aUser  x
                ago <- listA $ span /> getText -< x
                id <- span /> aItemId (=="link") -< x
                par <- span /> aItemId  (=="parent") -< x
                story <- span /> aItemId (`notElem` ["link","parent"]) -< x
                tt <- commentText -< x
                returnA -< Comment user id par story (intercalate "|" tt) (parseAgo (head ago) now) Normal

span = getXPathTrees "//div/span"

next doc = doc //> hasName "td" >>> hasAttrValue "class" (=="title")
           /> aHrefPrefix "/x?fnid" >>> getAttrValue "href"


aUser = aHrefPrefix "user" /> getText

aItemId inside = aHrefPrefix "item" >>> getId inside
aHrefPrefix x = hasName "a" >>> hasAttrValue "href" (x `isPrefixOf`)


getId f = (getChildren >>> hasText f) `guards` getAttrValue "href" >>> arr (read . stripItem)
stripItem = drop (length "item?id=")


data SParse = Title String String | Info String deriving Show

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


tcc = do t <- getCurrentTime
         e <- runX $ cc t doc
         return $ last e


