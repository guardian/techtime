module Lib where

import Control.Arrow ((&&&))
import Crypto.Hash.SHA1 (hash)
import Data.Array (Array, array, (!))
import qualified Data.ByteString as B
import Data.ByteString.Char8 (ByteString, pack)
import Data.Ix (range)
import Data.List (elemIndex, foldl1', permutations, zip)
import Data.List.NonEmpty (fromList)
import Data.Semigroup (Max(..), getMax, sconcat)

-- this data type will carry a sequence with its score
newtype Candidate = Candidate { getCandidate :: ([Int], Double) }

-- to make it a Semigroup, we need to install Candidate as Eq and Ord
instance Eq Candidate where
  (Candidate (_, x)) == (Candidate (_, y)) = x == y

instance Ord Candidate where
  (Candidate (_, x)) `compare` (Candidate (_, y)) | x == y    = EQ
                                                  | x <  y    = LT
                                                  | otherwise = GT

-- each permutation is assigned its score and pitted against the current winner
-- until the whole list is consumed
ms :: ([Int], Double)
ms = getCandidate . getMax . foldl1' (<>) . map (Max . Candidate . (id &&& score)) . permutations $ [0..99]

score :: [Int] -> Double
score xs = sum [pairscore n m xs | m <- [1..99], n <- [0..m], let x = posdist n m xs, x <= 3]

-- computes the SHA1
trace :: Int -> ByteString
trace = hash . pack . show

affinity :: Int -> Int -> Int
affinity n m = let
  x = trace n
  y = trace m
  in levenshtein x y

posdist :: Int -> Int -> [Int] -> Int
posdist n m as | Just i <- elemIndex n as
               , Just j <- elemIndex m as
               = if i < j then
                  length . take (j - i) . drop i $ as
                else
                  length . take (i - j) . drop j $ as


pairscore :: Int -> Int -> [Int] -> Double
pairscore n m xs = fromIntegral (affinity n m) / fromIntegral (posdist n m xs)

levenshtein :: ByteString -> ByteString -> Int
levenshtein xs ys = table ! (m,n)
  where
  (m,n) = (B.length xs, B.length ys)
  x     = array (1,m) (zip [1..] (B.unpack xs))
  y     = array (1,n) (zip [1..] (B.unpack ys))

  table :: Array (Int,Int) Int
  table = array bnds [(ij, dist ij) | ij <- range bnds]
  bnds  = ((0,0),(m,n))

  dist (0,j) = j
  dist (i,0) = i
  dist (i,j) = minimum [table ! (i-1,j) + 1, table ! (i,j-1) + 1,
    if x ! i == y ! j then table ! (i-1,j-1) else 1 + table ! (i-1,j-1)]
