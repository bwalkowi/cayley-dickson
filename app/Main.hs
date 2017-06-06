module Main where

import Lib
import Parser
import System.IO


main :: IO ()
main = do
  putStr "> "
  hFlush stdout --'>' może być zbuforowane
  input <- getLine
  putStrLn $ evalExpr input
  main

evalExpr :: String -> String
evalExpr input = case (readExpr input) of
  Right value -> show (simplify (eval value))
  Left err -> "Błąd parsera:\n" ++ (show err)
