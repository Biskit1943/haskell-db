{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module XmlDatabase
    ( readGuestXML )
where

import Text.XML.HXT.Core

data Guest = Guest { firstName, lastName :: String }
    deriving (Show, Eq)

getGuest = deep (isElem >>> hasName "guest") >>>
    proc x -> do
        fname <- text <<< atTag "fname" -< x
        lname <- text <<< atTag "lname" -< x
        returnA -< Guest { firstName = fname, lastName = lname}

atTag tag = deep (isElem >>> hasName tag)
text = getChildren >>> getText

readGuestXML = do
    guests <- runX (readDocument [withValidate no] "simple1.xml" >>> getGuest)
    print guests