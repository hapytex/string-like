{-# LANGUAGE FlexibleInstances, Safe #-}

{-|
Module      : Data.String.Like
Description : A module that aims to provide a uniform interface to string-like types.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

The module defines a typeclass that can be implemented to provide a uniform interface for 'String'-like objects (like 'String', 'LT.Text', etc.).

The typeclass itself has default implementations that convert the 'StringLike' item first to a lazy 'LT.Text', then performs the operation, and
converts results back to its 'StringLike' object. This is usually /not/ as efficient as an operation for that specific type. Therefore it is advisable
to implement the other functions as well. One can however decide to only implement 'fromText' and 'toText'; or 'toString'.

The module contains instances for 'String', 'T.Text', 'LT.Text', 'BS.ByteString' and 'LBS.ByteString'.
-}

module Data.String.Like(
    StringLike(
        empty, cons, snoc, uncons, unsnoc
      , length, compareLength
      , toString, fromChar
      , strMap, strConcat, strConcatMap, append
      , strAny, strAll, strNull
      , intercalate, intersperse
      , transpose, reverse
      , toLower, toUpper, toTitle
      , fromText, toText
    )
  , IsString(fromString)
  , convertStringLike
  ) where

import Prelude as P

import Control.Arrow(first, second)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Char as C
import Data.List as L
import Data.Function(on)
#if __GLASGOW_HASKELL__ < 803
import Data.Semigroup((<>))
#endif
import Data.String(IsString(fromString))
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding

-- | A typeclass that provides a uniform interface for string-like objects.
class IsString a => StringLike a where
    -- | Return an empty string-like object.
    empty :: a
    empty = fromText LT.empty
    -- | Create a stringlike object by prepending a 'Char' to an already
    -- existing string-like object.
    cons :: Char -> a -> a
    cons = _sandwich . LT.cons
    -- | Create a stringlike object by appending a 'Char' at the end of an
    -- already existing string-like object.
    snoc :: a -> Char -> a
    snoc a c = _sandwich (`LT.snoc` c) a
    -- | Unpack a stringlike object by obtaining the first character, and
    -- the rest of the string, given the string is non-empty. 'Nothing'
    -- otherwise.
    uncons :: a -> Maybe (Char, a)
    uncons = fmap (second fromText) . _throughText LT.uncons
    -- | Unpack a string-like object by obtaining te last character, and the
    -- string without the last character, given the string is non-empty.
    -- 'Nothing' otherwise.
    unsnoc :: a -> Maybe (a, Char)
    unsnoc = fmap (first fromText) . _throughText LT.unsnoc
    -- | Obtain the length of the string-like object.
    length :: a -> Int
    length = fromIntegral . _throughText LT.length
    -- | Compare the length of the string with the given length. Returns 'EQ' if
    -- the string has the same length, 'LT' if the string is shorter, and 'GT'
    -- if the string is longer. If the length is not explicitly stored, this
    -- function can stop from the moment the string-like object is exhausted, or
    -- the threshold has been reached.
    compareLength :: a -> Int -> Ordering
    compareLength = _throughText compareLength
    -- | Convert the given string-like object to a 'String'. If not specified,
    -- it will use 'toText', and then unpack the 'LT.Text' object in a 'String'.
    toString :: a -> String
    toString = LT.unpack . toText
    -- | Convert a given 'Char' to a string-like object containing the single
    -- character.
    fromChar :: Char -> a
    fromChar = fromString . pure
    -- | Concatenate the list of string-like objects to a string-like object.
    strConcat :: [a] -> a
    strConcat = fromText . LT.concat . map toText
    -- | Create a string-like object by mapping each character to another
    -- string-like object, and concatenate these.
    strConcatMap :: (Char -> a) -> a -> a
    strConcatMap = _sandwich . LT.concatMap . (toText .)
    -- | Check if any of the 'Char's in the string-like object satisfy a given
    -- condition.
    strAny :: (Char -> Bool) -> a -> Bool
    strAny = _throughText . LT.any
    -- | Check if all of the 'Char's of the string-like object satisfy a given
    -- condition.
    strAll :: (Char -> Bool) -> a -> Bool
    strAll = _throughText . LT.all
    -- | Check if the given string is empty.
    strNull :: a -> Bool
    strNull = _throughText LT.null
    -- | Append two string-like objects to a new string-like object.
    append :: a -> a -> a
    append a = fromText . on (<>) toText a
    -- | Map all the characters of a string-like object to a new string-like
    -- object.
    strMap :: (Char -> Char) -> a -> a
    strMap = _sandwich . LT.map
    -- | Inserts the given string-like object in between the string-like objects
    -- in the list. For example to make a comma-separated string.
    intercalate :: a -> [a] -> a
    intercalate t = fromText . LT.intercalate (toText t) . map toText
    -- | Inserts the given character in between the string-like objects in the
    -- list. For example to make a string of words.
    intersperse :: Char -> a -> a
    intersperse = _sandwich . LT.intersperse
    -- | Transposes the rows and columns of the list of string-like objects.
    transpose :: [a] -> [a]
    transpose = map fromText . LT.transpose . map toText
    -- | Calculate the reverse string of the given string.
    reverse :: a -> a
    reverse = _sandwich LT.reverse
    -- | Convert the given string-like object to its lowercase equivalent.
    toLower :: a -> a
    toLower = _sandwich LT.toLower
    -- | Convert the given string-like object to its uppercase equivalent.
    toUpper :: a -> a
    toUpper = _sandwich LT.toUpper
    -- | Convert the given string-like object to its title-case equivalent.
    toTitle :: a -> a
    toTitle = _sandwich LT.toTitle
    -- | Convert a 'LT.Text' object to the string-like object.
    fromText :: LT.Text -> a
    fromText = fromString . LT.unpack
    -- | Convert the string-like object to an 'LT.Text' object.
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

-- | Convert from one 'StringLike' type to another 'StringLike' type. This is
-- done through a lazy 'LT.Text'.
convertStringLike :: (StringLike a, StringLike b)
  => a -- ^ The 'StringLike' object to convert.
  -> b -- ^ The 'StringLike' object that is the equivalent of the given 'StringLike' object.
convertStringLike = fromText . toText
