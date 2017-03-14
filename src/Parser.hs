module Parser
  (
  ) where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
--Bazowane na https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing

{--
Plan:
(* [0 1] [0 1]) -> -1
(+ [2 3] [5 6]) -> [7 9]
--}

data Op = Add | Sub | Mul | Div deriving (Show)


data ReplVal = Operation Op --dodawanie itp
             | Number Double --tylko na liczbach rzeczywistych operujemy
             | NumList [ReplVal] --słodzik jak w Clojure
             | List [ReplVal] --s-expr do ewauacji
             deriving (Show)

--todo
readExpr :: String -> String
readExpr input = case parse parseExpr "S-expr" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value " ++ show val 


spaces :: Parser ()
spaces = skipMany1 space

--todo lepiej zrobić
parseOp :: Parser ReplVal
parseOp = do-- liftM recognizeOp $ oneOf "*/+-"
  op <- oneOf "*/+-"
  return $ case op of
    '+' -> Operation Add
    '*' -> Operation Mul
    '-' -> Operation Sub
    '/' -> Operation Div
    


parseNumber :: Parser ReplVal
parseNumber = liftM (Number . read) $ many1 digit --todo liczby ujemne (może z ~ jako nagacją)

parseList :: Parser ReplVal
parseList = liftM List $ sepBy parseExpr spaces

parseNumberList :: Parser ReplVal
parseNumberList = liftM NumList $ sepBy parseNumber spaces

parseExpr :: Parser ReplVal
parseExpr =  parseNumber --todo zdublowany kod
             <|> parseOp
             <|> do char '('
                    x <- try parseList 
                    char ')'
                    return x
             <|> do char '['
                    x <- try parseNumberList 
                    char ']'
                    return x
                  
