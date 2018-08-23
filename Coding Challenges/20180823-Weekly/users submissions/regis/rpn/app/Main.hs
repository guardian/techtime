module Main where

import Control.Monad (forever)
import Expr (run)

main :: IO ()
main = forever $ do
  putStrLn "Type some expression: "
  input <- getLine
  case run input of
    Left err -> putStrLn ("*BOOM* " ++ err)
    Right expr -> putStrLn ("In RPN: " ++ show expr)
