{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
import Test.QuickCheck
import CDParser
import Lib
--import Test.HUnit
--import Test.Framework
--import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad


{--
Notatka:
Testowanie przez generowanie jest nieprzyjemne i wcale nie lepsze od zwyczajnych testów jednostkowch.
Powód: Straciłem kilka godzin życia restartując komputer z powodu braku pamięci.
Haskel generuje więcej niż moja maszyna uciągnie i nie wiem jak to ograniczyć.

Uruchamiacie na własną odpowiedzialność!
--}


instance (Arbitrary a, Eq a, Fractional a) => Arbitrary (CDNum a) where
  arbitrary = do
    t <- arbitrary
    ts <- arbitrary
    return (Pair t ts)

prop_simplify ::  (CDNum Double) -> Bool
prop_simplify num =(simplify (Pair num neutralAdd)) == (simplify num)

prop_comutative :: ((CDNum Double) -> (CDNum Double) -> (CDNum Double)) ->(CDNum Double) -> (CDNum Double) -> Bool
prop_comutative f a b =(f a b) == (f b a)

prop_add :: (CDNum Double) -> (CDNum Double) -> Bool
prop_add = prop_comutative (+)


prop_sub :: (CDNum Double) -> (CDNum Double) -> Bool
prop_sub a b  = if a==b then True else not  (prop_comutative (-) a b)

--prop_multiply = prop_comutative (*) --nie jestem pewien czy to prawda
 
prop_laczne :: ((CDNum Double) -> (CDNum Double) -> (CDNum Double)) ->(CDNum Double) -> (CDNum Double) -> (CDNum Double) -> Bool
prop_laczne f a b c = (f (f a b) c) == (f a (f b c))

prop_add_laczne = prop_laczne (+)
prop_mul_laczne = prop_laczne (*) --nie jestem peiwen



main = do
  quickCheck prop_simplify
  quickCheck prop_add
  quickCheck prop_add_laczne
  quickCheck prop_mul_laczne
  quickCheck prop_sub
