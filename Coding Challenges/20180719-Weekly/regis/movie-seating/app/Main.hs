module Main where

import Lib

main :: IO ()
main = do
  putStr("Precompute the matrix...")
  let memo = prepare
  putStrLn(" Done! That was fast, Bill!")
  let bestest = ms memo
  putStrLn (show bestest)
