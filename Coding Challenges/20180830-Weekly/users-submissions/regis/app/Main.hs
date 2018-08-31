module Main where

import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Crypto.Random.Types (getRandomBytes)
import System.Random (randomRIO)
import Lib

main :: IO ()
main = loop
  where loop = do seq <- replicateM 100 (randomRIO ('a', 'z'))
                  let (x : y : xs) = seq
                  if valid ([x], [y], xs)
                    then case exploreQ seq of
                      [] -> loop
                      xs -> putStrLn ("Found " ++ show xs)
                    else loop
