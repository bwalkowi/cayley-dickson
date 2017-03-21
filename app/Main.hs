module Main where

import Lib
import CDParser --działa w replu nie działa wprzy budowaniu :/
import System.IO

main :: IO ()
--main = interact replFn
main = do
  putStr "> "
  hFlush stdout
  -- input <- readLn
  --putStrLn (replFn input)
  input <- getLine
  putStrLn $ replFn input
  main


replFn :: String -> String
replFn input = case (readExpr input) of
  Just out -> show (simplify (eval out))
  Nothing -> "ERR" --todo error handling
