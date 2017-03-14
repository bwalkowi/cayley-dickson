module Main where

import Lib
import CDParser
main :: IO ()
main = interact replFn

replFn :: String -> String
replFn input = case (readExpr input) of
  Just out -> show (simplify (eval out))
  Nothing -> "ERR"
