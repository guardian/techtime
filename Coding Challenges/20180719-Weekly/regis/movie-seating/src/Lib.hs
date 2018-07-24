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
import Data.List (zip)

-- this data type will carry a sequence with its score
newtype Candidate = Candidate { getCandidate :: ([Int], Double) }

-- Memoized matrix of affinities (we only use the bottom triangle)
type Memo = Array (Int, Int) Int

instance Show Candidate where
  show (Candidate p) = show p

minscore :: Double
minscore = 2365.33

-- each permutation is assigned its score and pitted against the current winner
-- until the whole list is consumed
ms :: Memo -> Candidate
ms memo = head . map f . filter (p . f) . permutations $ [99, 98..0]
  where f = Candidate . (id &&& score memo . sublists)
        p = (minscore <) . snd . getCandidate

permutations :: [a] -> [[a]]
permutations = foldr interleave [[]]
  where interleave x [] = []
        interleave x (ys:yss) = interleave' x ys ++ interleave x yss
        interleave' x [] = [[x]]
        interleave' x (y:ys) = (x:y:ys) : map (y:) (interleave' x ys)

sublists :: [Int] -> [(Int, [Int])]
sublists [x,y] = [(x, [y])]
sublists (x:y:xs) = (x, take 3 (y:xs)) : sublists (y:xs)

-- precompute the matrix
prepare :: Memo
prepare = array ((0,0),(99,99)) [((i,j), affinity i j) | i <- [1..99], j <- [0..i]]

score :: Memo -> [(Int, [Int])] -> Double
score memo xs = sum [pairscore memo n m d | (n,ys) <- xs, (d, m) <- zip [1..] ys]

-- computes the SHA1 and filters out digits
trace :: Int -> ByteString
trace = B.filter isAlpha . convertToBase Base16 . hashWith SHA1 . pack . show

affinity :: Int -> Int -> Int
affinity n m = let
  x = trace n
  y = trace m
  in levenshtein x y

pairscore :: Memo -> Int -> Int -> Int -> Double
pairscore memo n m d  = fromIntegral aff / fromIntegral d
  where aff = if n <= m then memo ! (m, n) else memo ! (n, m)

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

