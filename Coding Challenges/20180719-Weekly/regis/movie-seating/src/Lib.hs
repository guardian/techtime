module Lib where

import Control.Arrow ((&&&))
import Crypto.Hash (hashWith)
import Crypto.Hash.Algorithms (SHA1(..))
import Data.Array (Array, array, (!))
import Data.ByteArray.Encoding (convertToBase, Base(Base16))
import qualified Data.ByteString as B
import Data.ByteString.Char8 (ByteString, pack)
import Data.Word8 (isAlpha)
import Data.Ix (range)
import Data.List (foldl1', zip, permutations, (!!))
import Data.List.NonEmpty (fromList)
import Data.Semigroup (Max(..), getMax, sconcat)

-- this data type will carry a sequence with its score
newtype Candidate = Candidate { getCandidate :: (Array Int Int, Double) }

-- to make it a Semigroup, we need to install Candidate as Eq and Ord
instance Eq Candidate where
  (Candidate (_, x)) == (Candidate (_, y)) = x == y

instance Ord Candidate where
  (Candidate (_, x)) `compare` (Candidate (_, y)) | x == y    = EQ
                                                  | x <  y    = LT
                                                  | otherwise = GT

instance Show Candidate where
  show (Candidate p) = show p

-- each permutation is assigned its score and pitted against the current winner
-- until the whole list is consumed
ms :: Candidate
ms = getMax . foldl1' (<>) . map (Max . Candidate . (id &&& score) . array (0,99) . zip [0..]) . permutations $ [0..99]

score :: Array Int Int -> Double
score xs = sum [pairscore x y (m - n) | n <- [0..98], let o = min 99 (n+3), m <- [n+1..o], let x = xs ! n, let y = xs ! m]

-- computes the SHA1 and filters out digits
trace :: Int -> ByteString
trace = B.filter isAlpha . convertToBase Base16 . hashWith SHA1 . pack . show

affinity :: Int -> Int -> Int
affinity n m = let
  x = trace n
  y = trace m
  in levenshtein x y

pairscore :: Int -> Int -> Int -> Double
pairscore n m d = fromIntegral (affinity n m) / fromIntegral d

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

