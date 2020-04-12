{-# LANGUAGE FlexibleInstances, Safe #-}

module Data.String.Like where

import Prelude as P

import Control.Arrow(first, second)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Char as C
import Data.List as L
import Data.Function(on)
import Data.String(IsString(fromString))
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding

class IsString a => StringLike a where
    empty :: a
    empty = fromText LT.empty
    cons :: Char -> a -> a
    cons = _sandwich . LT.cons
    snoc :: a -> Char -> a
    snoc a c = _sandwich (`LT.snoc` c) a
    uncons :: a -> Maybe (Char, a)
    uncons = fmap (second fromText) . LT.uncons . toText
    unsnoc :: a -> Maybe (a, Char)
    unsnoc = fmap (first fromText) . LT.unsnoc . toText
    length :: a -> Int
    length = fromIntegral . _throughText LT.length
    compareLength :: a -> Int -> Ordering
    compareLength = compareLength . toText
    toString :: a -> String
    toString = LT.unpack . toText
    fromChar :: Char -> a
    fromChar = fromString . pure
    strConcat :: [a] -> a
    strConcat = fromText . LT.concat . map toText
    strConcatMap :: (Char -> a) -> a -> a
    strConcatMap = _sandwich . LT.concatMap . (toText .)
    strAny :: (Char -> Bool) -> a -> Bool
    strAny = _throughText . LT.any
    strAll :: (Char -> Bool) -> a -> Bool
    strAll = _throughText . LT.all
    strNull :: a -> Bool
    strNull = _throughText LT.null
    append :: a -> a -> a
    append a = fromText . on (<>) toText a
    strMap :: (Char -> Char) -> a -> a
    strMap = _sandwich . LT.map
    intercalate :: a -> [a] -> a
    intercalate t = fromText . LT.intercalate (toText t) . map toText
    intersperse :: Char -> a -> a
    intersperse = _sandwich . LT.intersperse
    transpose :: [a] -> [a]
    transpose = map fromText . LT.transpose . map toText
    reverse :: a -> a
    reverse = _sandwich LT.reverse
    toLower :: a -> a
    toLower = _sandwich LT.toLower
    toUpper :: a -> a
    toUpper = _sandwich LT.toUpper
    toTitle :: a -> a
    toTitle = _sandwich LT.toTitle
    fromText :: LT.Text -> a
    fromText = fromString . LT.unpack
    toText :: a -> LT.Text
    toText = LT.pack . toString
    {-# MINIMAL fromText, toText | toString #-}

_sandwich :: StringLike a => (LT.Text -> LT.Text) -> a -> a
_sandwich f = fromText . f . toText

_throughText :: StringLike a => (LT.Text -> b) -> a -> b
_throughText = (. toText)

instance StringLike [Char] where
    empty = []
    cons = (:)
    snoc = (. pure) . (++)
    uncons = L.uncons
    unsnoc [] = Nothing
    unsnoc (x:xs) = Just (go x xs)
        where go y [] = ([], y)
              go y (z:zs) = let ~(ws,w) = go z zs in (y:ws,w)
    length = P.length
    compareLength [] n
      | n < 0 = GT
      | n > 0 = LT
      | otherwise = EQ
    compareLength xs n
      | [] <- dn = LT
      | [_] <- dn = EQ
      | otherwise = GT
      where dn = drop (n-1) xs
    toString = id
    fromChar = pure
    strConcat = concat
    strConcatMap = concatMap
    strAny = any
    strAll = all
    strNull = null
    append = (++)
    strMap = map
    intercalate = L.intercalate
    intersperse = L.intersperse
    transpose = L.transpose
    reverse = P.reverse
    toLower = map C.toLower
    toUpper = map C.toUpper


instance StringLike T.Text where
    empty = T.empty
    cons = T.cons
    snoc = T.snoc
    uncons = T.uncons
    unsnoc = T.unsnoc
    length = T.length
    compareLength = T.compareLength
    toString = T.unpack
    fromChar = T.singleton
    strConcat = T.concat
    strConcatMap = T.concatMap
    strAny = T.any
    strAll = T.all
    strNull = T.null
    append = T.append
    strMap = T.map
    intercalate = T.intercalate
    intersperse = T.intersperse
    transpose = T.transpose
    reverse = T.reverse
    toLower = T.toLower
    toUpper = T.toUpper
    toTitle = T.toTitle
    toText = LT.fromStrict
    fromText = LT.toStrict

instance StringLike LT.Text where
    empty = LT.empty
    cons = LT.cons
    snoc = LT.snoc
    uncons = LT.uncons
    unsnoc = LT.unsnoc
    length = fromIntegral . LT.length
    compareLength = (. fromIntegral) . LT.compareLength
    toString = LT.unpack
    fromChar = LT.singleton
    strConcat = LT.concat
    strConcatMap = LT.concatMap
    strAny = LT.any
    strAll = LT.all
    strNull = LT.null
    append = LT.append
    strMap = LT.map
    intercalate = LT.intercalate
    intersperse = LT.intersperse
    transpose = LT.transpose
    reverse = LT.reverse
    toLower = LT.toLower
    toUpper = LT.toUpper
    toTitle = LT.toTitle
    toText = id
    fromText = id

instance StringLike BS.ByteString where
    empty = BS.empty
    cons = BS.cons
    snoc = BS.snoc
    uncons = BS.uncons
    unsnoc = BS.unsnoc
    length = BS.length
    fromChar = BS.singleton
    strConcat = BS.concat
    strConcatMap = BS.concatMap
    strAny = BS.any
    strAll = BS.all
    strNull = BS.null
    append = BS.append
    strMap = BS.map
    intercalate = BS.intercalate
    intersperse = BS.intersperse
    transpose = BS.transpose
    reverse = BS.reverse
    toLower = BS.map C.toLower
    toUpper = BS.map C.toUpper
    toText = decodeUtf8 . LBS.fromStrict
    fromText = LBS.toStrict . encodeUtf8

instance StringLike LBS.ByteString where
    empty = LBS.empty
    cons = LBS.cons
    snoc = LBS.snoc
    uncons = LBS.uncons
    unsnoc = LBS.unsnoc
    length = fromIntegral . LBS.length
    fromChar = LBS.singleton
    strConcat = LBS.concat
    strConcatMap = LBS.concatMap
    strAny = LBS.any
    strAll = LBS.all
    strNull = LBS.null
    append = LBS.append
    strMap = LBS.map
    intercalate = LBS.intercalate
    intersperse = LBS.intersperse
    transpose = LBS.transpose
    reverse = LBS.reverse
    toLower = LBS.map C.toLower
    toUpper = LBS.map C.toUpper
    toText = decodeUtf8
    fromText = encodeUtf8

convertStringLike :: (StringLike a, StringLike b) => a -> b
convertStringLike = fromText . toText
