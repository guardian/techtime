module Lib where

import Control.Monad (replicateM)
import Crypto.Hash (Digest, hash)
import Crypto.Hash.Algorithms (SHA1)
import Data.ByteString.Char8 (pack)

type Triple a = (a, a, a)
type Quadruple a = (a, a, a, a)

-- | Generates sublists of length 3 from a list with at least 3 elements
triples :: [a] -> [Triple [a]]
triples xs = [(as, bs, cs) | n <- [1..l - 2], m <- [1..l - n - 1], let (as, xs') = splitAt n xs, let (bs, cs) = splitAt m xs']
  where l = length xs

-- | Generates sublists of length 4 from a list with at least 4 elements
quadruples :: [a] -> [Quadruple [a]]
quadruples xs = [(as, bs, cs, ds) | n <- [1..l - 2], m <- [1..l - n - 1], o <- [1..l - (m + n) - 1], let (as, xs') = splitAt n xs, let (bs, xs'') = splitAt m xs', let (cs, ds) = splitAt o xs'']
  where l = length xs

-- | Generates all permutations of a triple
perms :: Triple a -> [Triple a]
perms (a, b, c) = [ (a, b, c)
                  , (a, c, b)
                  , (b, a, c)
                  , (b, c, a)
                  , (c, a, b)
                  , (c, b, a)
                  ]

-- | Generates all permutations of a triple
permsQ :: Quadruple a -> [Quadruple a]
permsQ (a, b, c, d) = [ (a, b, c, d) , (a, c, b, d) , (b, a, c, d) , (b, c, a, d) , (c, a, b, d) , (c, b, a, d)
                      , (a, b, d, c) , (a, c, d, b) , (b, a, d, c) , (b, c, d, a) , (c, a, d, b) , (c, b, d, a)
                      , (a, d, b, c) , (a, d, c, b) , (b, d, a, c) , (b, d, c, a) , (c, d, a, b) , (c, d, b, a)
                      , (a, d, b, c) , (d, a, c, b) , (d, b, a, c) , (d, b, c, a) , (d, c, a, b) , (d, c, b, a)
                      ]

-- | Checks is a string ends with 0
valid :: Triple String -> Bool
valid (a, b, c) = (== '0') . last . hashseq $ a ++ b ++ c

-- | Checks is a string ends with 0
validQ :: Quadruple String -> Bool
validQ (a, b, c, d) = (== '0') . last . hashseq $ a ++ b ++ c ++ d

-- | Computes the SHA1 of a string
sha1 :: String -> Digest SHA1
sha1 = hash . pack

-- | Witnesses a valid sequence
hashseq :: String -> String
hashseq = show . sha1

explore :: String -> [Triple String]
explore = map head . filter (all valid . tail) . map perms . triples

exploreQ :: String -> [Quadruple String]
exploreQ = map head . filter (all validQ . tail) . map permsQ . quadruples