module Parser
  (readExpr
  , eval
  ) where

import Lib
import Text.ParserCombinators.Parsec.Number
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
--Bazowane na https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing

{--
Plan:
(* [0 1] [0 1]) -> -1
(+ [2 3] [5 6]) -> [7 9]
--}


--Operacje rozpoznawane przez Parser
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
parseOp = do
  op <- oneOf "*/+-~rc"
  return $ case op of
    '+' -> Operation Add
    '*' -> Operation Mul
    '-' -> Operation Sub
    '/' -> Operation Div
    '~' -> Operation Neg
    'r' -> Operation Recip
    'c' -> Operation Conjugate
    


parseNumber :: Parser ReplVal
parseNumber = liftM (Number . read) $ many1 (oneOf "-0123456789." ) --todo ulepszyć

parseList :: Parser ReplVal
parseList = liftM List $ sepBy parseExpr spaces

parseNumberList :: Parser ReplVal
parseNumberList = liftM NumList $ sepBy parseNumber spaces

parseExpr :: Parser ReplVal
parseExpr =  parseOp --todo zdublowany kod
             <|> parseNumber
             <|> do char '('
                    x <- try parseList 
                    char ')'
                    return x
             <|> do char '['
                    x <- try parseNumberList 
                    char ']'
                    return x
                  


readExpr :: String -> Either ParseError ReplVal
readExpr input = parse parseExpr "S-expr" input


--todo niech parser nie ewaluuje
eval ::  ReplVal -> CDNum Double
eval (Number d) = Base d
eval (NumList l) = constructFromList (map (\x-> case x of Number d-> d) l) --todo zakładamy że w NumberList są same liczby
eval (List l) = let Operation op = (head l) --todo wydłubać sobie oczy
                    in
                  evalOp op (tail l)

evalOp :: Op -> [ReplVal] -> CDNum Double
evalOp Add [val1, val2] = (+) (eval val1) (eval val2)
evalOp Sub [val1, val2] = (-) (eval val1) (eval val2)
evalOp Mul [val1, val2] = (*) (eval val1) (eval val2)
evalOp Div [val1, val2] = (/) (eval val1) (eval val2)
evalOp Neg [v] = negate (eval v)
evalOp Recip [v] = recip (eval v)
evalOp Conjugate [v] = conjugate (eval v)








