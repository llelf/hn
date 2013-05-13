{-# LANGUAGE OverloadedStrings, UnicodeSyntax, Arrows, TupleSections #-}
module HN.Comment (Comment(..), commentToText, cc) where

import Prelude hiding (span)
import Prelude.Unicode
import Data.Aeson
import Data.Time
import Text.Pandoc
import Data.List
import Data.List.Split
import HN.Parsing

type ID = Integer

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
parseAgo ∷ String → UTCTime → UTCTime
parseAgo s now
    | (sn : q : "ago" : _) <- words s = addUTCTime (negate $ fromIntegral (read sn :: Int) * qToSecs q) now
    | otherwise                       = now
    where
      qToSecs x | "minute" `isPrefixOf` x = 60
                | "hour" `isPrefixOf` x   = 60 * qToSecs "minute"
                | "day" `isPrefixOf` x    = 24 * qToSecs "hour"



cc ∷ ArrowXml cat ⇒ UTCTime → cat a XmlTree → cat a Comment
cc now doc =
    doc >>> xpath "//html/body/center/table/tr/td/table/tr/td[@class='default']"
        >>> proc x ->
            do user ← spanEl /> aUser ⤙ x
               ago <- listA $ spanEl /> getText -< x
               id <- spanEl /> aItemId (=="link") -< x
               par <- spanEl /> aItemId  (=="parent") -< x
               story <- spanEl /> aItemId (`notElem` ["link","parent"]) -< x
               tt <- commentText -< x
               returnA -< Comment user id par story tt (parseAgo (head ago) now) Normal


spanEl ∷ ArrowXml cat ⇒ cat XmlTree XmlTree
spanEl = xpath "//div/span"



data CT = CTPar | CTIn { unCT :: Inline } deriving Eq

commentText ∷ ArrowXml cat ⇒ cat XmlTree [Block]
commentText = listA (getChildren
                     ⋙ xpath "span/font"
                     ⋙ listA (getChildren
                              ⋙ ((hasName "p" ⋙ constA CTPar)
                                 <+> (hasName "p" /> par >>^ CTIn)
                                 `orElse` (par >>^ CTIn)))
                    >>^ (map (Para ∘ map unCT) ∘ splitOn [CTPar]))
              >>^ concat

    where par ∷ ArrowXml cat ⇒ cat XmlTree Inline
          par = (((getText >>^ makeTxt)
                  <+> (hasName "a" ⋙ getAttrValue "href" >>^ makeLink)
                  <+> (hasName "i" /> getText >>^ makeEmph)
                 )
                 `orElse` (arr makeUnknownNote)
                )

          makeTxt = Str ∘ sanitize
          makeLink = Link [] ∘ (, "") ∘ sanitize
          makeEmph = Emph ∘ (:[]) ∘ Str ∘ sanitize
          makeUnknownNote = Note ∘ (:[]) ∘ Para ∘ (:[]) ∘ Str ∘ show
          sanitize = id

