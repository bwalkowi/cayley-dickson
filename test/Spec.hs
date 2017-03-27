{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
import Test.QuickCheck
import CDParser
import Lib
--import Test.HUnit
--import Test.Framework
--import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad


instance (Arbitrary a, Eq a, Fractional a) => Arbitrary (CDNum a) where
  arbitrary = do
    t <- arbitrary
    ts <- arbitrary
    return (Pair t ts)

prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = reverse (xs++ys) ==reverse ys++ reverse xs

prop_simplify ::  (CDNum Double) -> Bool
prop_simplify num =(simplify (Pair num neutralAdd)) == (simplify num)

main = do
  quickCheck prop_revapp
  quickCheck prop_simplify 

