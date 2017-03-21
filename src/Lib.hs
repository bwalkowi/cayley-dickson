module Lib
    ( someFunc
    ,  CDNum(..)
    , CDOps(..)
    , simplify
    , constructFromList
    ) where

someFunc :: IO ()
someFunc = putStrLn "Elo"


class (Fractional a) => CDOps a where
  neutralAdd :: a
  neutralMul :: a
  conjugate :: a -> a
  normSqr :: a -> a
  normSqr a = a * (conjugate a)

instance CDOps Double where
  neutralAdd = 0
  neutralMul = 1
  conjugate a = a

data CDNum a = Base a | Pair (CDNum a)  (CDNum a) deriving (Eq)

instance (Show a) => Show (CDNum a) where
  show (Base a) = show a
  show (Pair a b) = "["++(show a) ++" "++ (show b) ++ "]"
  
instance (CDOps a) => Num (CDNum a) where
  (+) = cdAdd
  negate = cdNegative
  (*) = cdMultiply
  fromInteger a = Base (fromInteger a)
  signum = cdSignum
  abs = cdNormSqr --todo pierwiastek
  
instance (CDOps a) => Fractional (CDNum a) where
  recip = cdRecip
  fromRational a = Base (fromRational a)
  
instance (CDOps a) => CDOps (CDNum a) where
  neutralAdd = cdNeutralAdd
  neutralMul = cdNeutralMultiply
  conjugate = cdConjugate
  normSqr = cdNormSqr

--[1] -> [1]
--[1 2] -> [1 2]
--[1 2 3 4] -> [[1 2] [3 4]]
--[1 2 3 4 5 6 7 8] -> [[1 2 3 4] [5 6 7 8]] -> [[[1 2] [3 4]] [[5 6] [7 8]]]
--todo: w razie gdyby lista nie była długości 2^n trzeba będzie dopełnić zerami
constructFromList :: (CDOps a) => [a] -> CDNum a
constructFromList lst = cnstrFromList  (map Base lst)

partition :: Int -> [a] -> [[a]]
partition _ [] = []
partition n xs = (take n xs) : (partition n (drop n xs))

cnstrFromList :: [CDNum a] -> CDNum a
cnstrFromList [x] = x
cnstrFromList [a, b] = Pair a b
cnstrFromList xs = cnstrFromList (map cnstrFromList (partition 2 xs))

qIsNeutralAdd :: (CDOps a, Eq a) => (CDNum a) -> Bool
qIsNeutralAdd (Base a) = neutralAdd == a
qIsNeutralAdd (Pair a b) = (qIsNeutralAdd a) && (qIsNeutralAdd b)

simplify :: (Eq a, CDOps a) => CDNum a -> CDNum a
simplify (Base a) = Base a
simplify (Pair a b) = if (qIsNeutralAdd b)
  then simplify a
  else (Pair a b)

cdSignum :: (CDOps a) => CDNum a -> CDNum a --todo czy na pewno poprawne?
cdSignum a = cdDivide (cdMultiply a a) (cdNormSqr a)

cdExtend :: (CDOps a) => CDNum a -> CDNum a
cdExtend a = Pair a (Base neutralAdd)

callWithEqualNestedness :: (CDOps a) => (CDNum a-> CDNum a-> CDNum a) -> CDNum a-> CDNum a-> CDNum a
callWithEqualNestedness fn (Base a1) (Pair a2 b2) =
  callWithEqualNestedness fn (cdExtend (Base a1)) (Pair a2 b2)
callWithEqualNestedness fn (Pair a1 b1) (Base a2) =
  callWithEqualNestedness fn (Pair a1 b1) (cdExtend (Base a2))
callWithEqualNestedness fn a b = fn a b

cdAdd :: (CDOps a) => CDNum a -> CDNum a -> CDNum a
cdAdd (Base a) (Base b) = Base (a+b)
cdAdd (Pair a1 b1) (Pair a2 b2) = Pair (cdAdd a1 a2) (cdAdd b1 b2)
cdAdd a b = callWithEqualNestedness cdAdd a b

cdNeutralAdd :: (CDOps a) => CDNum a
cdNeutralAdd =  Pair (Base neutralAdd) (Base neutralAdd)

cdNegative :: (CDOps a) => CDNum a-> CDNum a
cdNegative (Base a) = Base (-a)
cdNegative (Pair a b) = Pair (cdNegative a) (cdNegative b)

cdSubstract ::  (CDOps a) => CDNum a -> CDNum a -> CDNum a
cdSubstract a b = cdAdd a (cdNegative b)

cdMultiply :: (CDOps a) => CDNum a -> CDNum a -> CDNum a
cdMultiply (Base a) (Base b) = Base (a*b)
cdMultiply (Pair a1 b1) (Pair a2 b2) = Pair
  (cdSubstract (cdMultiply a1 a2) (cdMultiply (cdConjugate b2) b1))
  (cdAdd (cdMultiply a1 b2) (cdMultiply (cdConjugate a2) b1))
cdMultiply a b = callWithEqualNestedness cdMultiply a b

cdNeutralMultiply :: (CDOps a) => CDNum a
cdNeutralMultiply = Pair (Base neutralMul) (Base neutralAdd)

cdRecip ::(CDOps a) => CDNum a-> CDNum a
cdRecip (Base a) = Base (recip a)
cdRecip (Pair a b) = Pair
  (cdDivide (cdConjugate a) (cdAdd (cdNormSqr a) (cdNormSqr b)))
  (cdNegative (cdDivide b (cdAdd (cdNormSqr a) (cdNormSqr b))))

cdDivide :: (CDOps a) => CDNum a-> CDNum a -> CDNum a
cdDivide a b = cdMultiply a (cdRecip b)

cdConjugate :: (CDOps a) => CDNum a -> CDNum a
cdConjugate (Base a) = Base (conjugate a)
cdConjugate (Pair a b) = Pair (cdConjugate a) (cdNegative b)

cdNormSqr :: (CDOps a) =>CDNum a -> CDNum a
cdNormSqr  a = cdMultiply (cdConjugate a) a
