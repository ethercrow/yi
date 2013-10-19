{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

-- Consider splitting off as a separate package
-- Copyright (c) 2008 Gustav Munkby
-- Copyright (c) 2008 Jean-Philippe Bernardy

-- | This module defines a Rope representation.

-- While the representation are T.Texts stored in a finger tree, the indices
-- are actually in number of characters.

-- This is currently based on utf8-string, but a couple of other packages might be
-- better: text, compact-string. 

-- At the moment none of them has a lazy
-- implementation, which forces us to always export plain Strings.
-- (Utf8-string does not have a proper newtype)
 
module Data.Rope (
   Rope,
 
   -- * Conversions to Rope
   fromString,
 
   -- * Conversions from Rope
   toString, toReverseString,
 
   -- * List-like functions
   null, empty, singleton, take, drop,  length, reverse, countNewLines,

   split, splitAt, splitAtLine,

   append, concat,
 
   -- * IO
   readFile, writeFile,

   -- * Low level functions
   splitAtChunkBefore
  ) where
 
import Prelude hiding (null, head, tail, length, take, drop, splitAt, head, tail, foldl, reverse, readFile, writeFile, concat)

import Data.Binary
import Data.Char (ord)
import qualified Data.FingerTree as FT
import Data.FingerTree hiding (singleton, null, empty, reverse, split)
import Data.Function (on)
import qualified Data.List as L
import Data.Monoid
import Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LTIO

defaultChunkSize :: Int
defaultChunkSize = 128 -- in chars! (chunkSize requires this to be <= 255)

-- The FingerTree does not store measurements for single chunks, which
-- means that the length of chunks often have to be recomputed.
mkChunk :: T.Text -> Chunk
mkChunk s = Chunk (fromIntegral $ T.length s) s

data Chunk = Chunk {
    chunkSize :: {-# UNPACK #-} !Word8
  , fromChunk :: {-# UNPACK #-} !T.Text
  } deriving (Eq, Show)

data Size = RopeSize {
    charCount :: {-# UNPACK #-} !Int
  , newlineCount :: {-# UNPACK #-} !Int
  } deriving Show

instance Monoid Size where
    mempty = RopeSize 0 0
    mappend (RopeSize c1 l1) (RopeSize c2 l2) = RopeSize (c1 + c2) (l1 + l2)
 
newtype Rope = Rope { fromRope :: FingerTree Size Chunk }
   deriving (Show)

instance Eq Rope where
    (==) = (==) `on` toLazyText
 
instance Monoid Rope where
    mempty = empty
    mappend = append

(-|) :: Chunk -> FingerTree Size Chunk -> FingerTree Size Chunk
b -| t | chunkSize b == 0 = t
       | otherwise        = b <| t
 
(|-) :: FingerTree Size Chunk -> Chunk -> FingerTree Size Chunk
t |- b | chunkSize b == 0 = t
       | otherwise        = t |> b
 
instance Measured Size Chunk where
   measure (Chunk l s) =
       RopeSize (fromIntegral l) -- note that this is the length in characters, not bytes.
                (T.count (T.singleton '\n') s)

-- | The 'Foldable' instance of 'FingerTree' only defines 'foldMap',
-- so the 'foldr' needed for 'toList' is inefficient,
-- and can cause stack overflows. So, we roll our own (somewhat inefficient)
-- version of 'toList' to avoid this.
toList :: Measured v a => FingerTree v a -> [a]
toList t = case viewl t of
              c :< cs -> c : toList cs
              EmptyL -> []

toLazyText :: Rope -> LT.Text
toLazyText = LT.fromChunks . fmap fromChunk . toList . fromRope

reverseChunk :: Chunk -> Chunk
reverseChunk (Chunk size text) = Chunk size (T.reverse text)

reverse :: Rope -> Rope
reverse = Rope . fmap' reverseChunk . FT.reverse . fromRope
 
toReverseString :: Rope -> String
toReverseString = L.concatMap (T.unpack . T.reverse . fromChunk) . toList . FT.reverse . fromRope

toString :: Rope -> String
toString = LT.unpack . toLazyText
 
fromLazyText :: LT.Text -> Rope
fromLazyText = Rope . toTree FT.empty
   where
     toTree !acc b | LT.null b = acc
                   | otherwise = let (h,t) = LT.splitAt (fromIntegral defaultChunkSize) b
                                     !chunk = mkChunk $ T.concat $ LT.toChunks h
                                 in toTree (acc |> chunk) t
 
fromString :: String -> Rope
fromString = Rope . toTree FT.empty
   where
     toTree !acc [] = acc
     toTree !acc b  = let (h,t) = L.splitAt defaultChunkSize b
                          !chunk = mkChunk $ T.pack h
                      in toTree (acc |> chunk) t

null :: Rope -> Bool
null (Rope a) = FT.null a
 
empty :: Rope
empty = Rope FT.empty
 
singleton :: Char -> Rope
singleton c = fromString [c]

-- | Get the length of the string.
--   (This information is cached, so O(1) amortized runtime.)
length :: Rope -> Int
length = charCount . measure . fromRope

-- | Count the number of newlines in the strings.
--   (This information is cached, so O(1) amortized runtime.)
countNewLines :: Rope -> Int
countNewLines = newlineCount . measure . fromRope

-- | Append two strings by merging the two finger trees.
append :: Rope -> Rope -> Rope
append (Rope a) (Rope b) = Rope $
    case (FT.viewr a, FT.viewl b) of
      (EmptyR, _) -> b
      (_, EmptyL) -> a
      (l :> Chunk len1 x1, Chunk len2 x2 :< r) ->
          if fromIntegral len1 + fromIntegral len2 < defaultChunkSize
          then l >< FT.singleton (Chunk (len1 + len2) (x1 `T.append` x2)) >< r
          else a >< b

concat :: [Rope] -> Rope
concat = L.foldl' append empty
 
take, drop :: Int -> Rope -> Rope
take n = fst . splitAt n
drop n = snd . splitAt n
 
-- | Split the string at the specified position.
splitAt :: Int -> Rope -> (Rope, Rope)
splitAt n (Rope t) =
   case FT.viewl c of
     Chunk len x :< r | n' /= 0 ->
       let (lx, rx) = T.splitAt n' x
       in (Rope $ l |> Chunk (fromIntegral n') lx,
           Rope $ Chunk (len - fromIntegral n') rx -| r)
     _ -> (Rope l, Rope c)
   where
     (l, c) = FT.split ((> n) . charCount) t
     n' = n - charCount (measure l)

-- | Split the rope on a chunk, so that the desired
--   position lies within the first chunk of the second rope.
splitAtChunkBefore :: Int -> Rope -> (Rope, Rope)
splitAtChunkBefore n (Rope t) =
  let (l, c) = FT.split ((> n) . charCount) t in (Rope l, Rope c)

-- | Split before the specified line. Lines are indexed from 0.
splitAtLine :: Int -> Rope -> (Rope, Rope)
splitAtLine n | n <= 0     = \r -> (empty, r)
              | otherwise = splitAtLine' (n - 1)

-- | Split after the specified line. Lines are indexed from 0.
splitAtLine' :: Int -> Rope -> (Rope, Rope)
splitAtLine' n (Rope t) =
   case FT.viewl c of
     ch@(Chunk _ x) :< r ->
       let (lx, rx) = grabMoreLines neededNewlines x
           neededNewlines = n + 1 - newlineCount (measure l)
       in (Rope $ l |- mkChunk lx, Rope $ mkChunk rx -| r)
     _ -> (Rope l, Rope c)
   where
     (l, c) = FT.split ((n <) . newlineCount) t

split :: Char -> Rope -> [Rope]
split c = map fromLazyText . LT.split (== c) . toLazyText

grabMoreLines :: Int -> T.Text -> (T.Text, T.Text)
grabMoreLines i s =
    if i <= L.length splits
    then let (l, r) = splits !! (i - 1)
         -- take one extra byte so that the newline is found on the left.
         in (T.append l (T.singleton '\n'), T.drop 1 r)
    else (s, T.empty)
    where splits = T.breakOnAll (T.singleton '\n') s

instance Binary Rope where
     put = put . toString
     get = fromString `fmap` get

writeFile :: FilePath -> Rope -> IO ()
writeFile f = LTIO.writeFile f . toLazyText

readFile :: FilePath -> IO Rope
readFile f = fromLazyText `fmap` LTIO.readFile f
