
{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
import Test.QuickCheck
--import Test.HUnit
--import Test.Framework
--import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
--import Utils



prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = reverse (xs++ys) == reverse xs ++ reverse ys

main = quickCheck prop_revapp
