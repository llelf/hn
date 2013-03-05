{-# LANGUAGE Arrows, NoMonomorphismRestriction, TupleSections, UnicodeSyntax,
  OverloadedStrings #-}

module HN.NewComments (NCPage(..),Comment(..),runX,parse) where

-- html>body>center>table> tr>td>table>tr> td[class=default]

import Prelude hiding (span)
import Prelude.Unicode
import Control.Arrow.Unicode
import Control.Applicative
import Text.XML.HXT.Core
import Text.HandsomeSoup
import Data.List hiding (span)
import Control.Monad (join)
import Data.Aeson
import Data.Time

import Text.Pandoc

import HN.Parsing


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
                 comms <- runX $ cc now doc
                 --[nxt] <- runX $ next doc
                 return $ NCPage comms ""

pp file = do f <- readFile file
             t <- getCurrentTime
             p <- parse t f
             return p


--yo ∷ Comment → ByteString
yo = encode


pComment = getChildren >>> hasName "font" /> getText

type ID = Int

data Voted = Normal | Downvoted | Dead
             deriving Show


data AgoUnit = AgoM | AgoH | AgoD

data Comment = Comment { cUser :: String, cId :: ID,
                         cParent :: ID, cStory :: ID, cText :: [Block],
                         cTime :: UTCTime,
                         cVoted :: Voted }
               deriving Show


instance ToJSON Comment where
    toJSON c = object [ "user" .= cUser c, "parent" .= cParent c, "story" .= cStory c,
                        "time" .= cTime c, "text" .= commentToText c ]



commentToPandoc (Comment user _ _ _ text time _) = Pandoc (Meta [] [] []) text

commentToText :: Comment -> String
commentToText = writeMarkdown def . commentToPandoc


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
commentText ∷ ArrowXml cat ⇒ cat XmlTree [Block]
commentText = listA $ getChildren
              ⋙ xpath "span/font"
              /> listA (getChildren `when` hasName "p"
                        ⋙ par)
              ⋙ arr Para
    where par ∷ ArrowXml cat ⇒ cat XmlTree Inline
          par = (((getText ⋙ arr makeTxt)
                  <+> (hasName "a" ⋙ getAttrValue "href" ⋙ arr makeLink)
                  <+> (hasName "i" /> getText ⋙ arr makeEmph)
                 )
                 `orElse` (arr makeUnknownNote)
                )

          makeTxt = Str ∘ sanitize
          makeLink = Link [] ∘ (, "") ∘ sanitize
          makeEmph = Emph ∘ (:[]) ∘ Str ∘ sanitize
          makeUnknownNote = Note ∘ (:[]) ∘ Para ∘ (:[]) ∘ Str ∘ show
          sanitize = unwords ∘ words



cc ∷ ArrowXml cat ⇒ UTCTime → cat a XmlTree → cat a Comment
cc now doc =
    doc >>> xpath "//html/body/center/table/tr/td/table/tr/td[@class='default']"
        >>> proc x ->
            do user ← span /> aUser ⤙ x
               ago <- listA $ span /> getText -< x
               id <- span /> aItemId (=="link") -< x
               par <- span /> aItemId  (=="parent") -< x
               story <- span /> aItemId (`notElem` ["link","parent"]) -< x
               tt <- commentText -< x
               returnA -< Comment user id par story tt (parseAgo (head ago) now) Normal

span = xpath "//div/span"

next doc = doc //> hasName "td" >>> hasAttrValue "class" (=="title")
           /> aHrefPrefix "/x?fnid" >>> getAttrValue "href"



