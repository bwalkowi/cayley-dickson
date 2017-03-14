module Parser
  (
  ) where

import Lib
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
--Bazowane na https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing

{--
Plan:
(* [0 1] [0 1]) -> -1
(+ [2 3] [5 6]) -> [7 9]
--}

data Op = Add | Sub | Mul | Div | Neg | Recip |Conjugate deriving (Show)


data ReplVal = Operation Op --dodawanie itp
             | Number Double --tylko na liczbach rzeczywistych operujemy
             | NumList [ReplVal] --słodzik jak w Clojure (todo: zakładamy że ReplVale to same liczby!)
             | List [ReplVal] --s-expr do ewauacji
             deriving (Show)

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
                  


--todo
readExpr :: String -> Maybe ReplVal
readExpr input = case parse parseExpr "S-expr" input of
  Left err -> Nothing
  Right val -> Just val 



--todo osobny moduł dla Evala
eval ::  ReplVal -> CDNum Double
eval (Number d) = Base d
eval (NumList l) = constructFromList (map (\x-> case x of Number d-> d) l) --todo zakładamy że w NumberList są same liczby
eval (List l) = let Operation op = (head l) --todo wydłubać sobie oczy
                    in
                  evalOp op (tail l)
--eval (Operation op)

evalOp :: Op -> [ReplVal] -> CDNum Double
evalOp Add [val1, val2] = (+) (eval val1) (eval val2)
evalOp Sub [val1, val2] = (-) (eval val1) (eval val2)
evalOp Mul [val1, val2] = (*) (eval val1) (eval val2)
evalOp Div [val1, val2] = (/) (eval val1) (eval val2)
evalOp Neg [v] = negate (eval v)
evalOp Recip [v] = recip (eval v)
evalOp Conjugate [v] = conjugate (eval v)
