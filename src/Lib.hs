module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn ("someFunc " ++ (show d) ++ "\n  " ++(show eqq))

d :: CDnum Double
d = divCD (Pair (Base 0.0) (Base 1.0))  (Base 320.0)

eqqq = 2
eqq:: {-- --}CDnum Double
eqq = {-- getFirstBase $ --} sqNormCD (Pair
                 (Pair (Base 10.0) (Base 5.0))
                 ( Base 13.0 ))
{--
Konstrukcja Cayleya-Dicksona

Konstrukcja Cayleya-Dicksona jest metodą rozszerzania unormowanej przestrzeni liniowej przez tworzenie par jej elementów ( a , b ) {\displaystyle (a,\;b)} {\displaystyle (a,\;b)}, a następnie definiowanie działań w następujący sposób:
--}

data CDnum a = Pair (CDnum a) (CDnum a) | Base a deriving (Eq, Show)


fillMissing :: (Operations a) => (CDnum a-> CDnum a -> CDnum a) -> CDnum a-> CDnum a -> CDnum a
fillMissing fn (Pair a b) (Base c) = fillMissing fn (Pair a b) (Pair (Base c) (neutralAddCD (Base  c)))
fillMissing fn (Base a) (Pair c d) = fillMissing fn (Pair (Base a) (neutralAddCD (Base a))) (Pair c d)
fillMissing fn a b = fn a b

simplify :: (Operations a) => CDnum a -> CDnum a
simplify (Base a) = Base a
simplify (Pair a b) = if (neutralAddCD b) == b  then simplify a  else (Pair a b)

getFirstBase :: (Operations a) => CDnum a-> a
getFirstBase (Base a) =a
getFirstBase (Pair a _) = getFirstBase a

addCD :: (Operations a) =>  CDnum a -> CDnum a -> CDnum a 
addCD (Base a) (Base b) = Base (add a b)
addCD (Pair a1 b1) (Pair a2 b2) = Pair (addCD a1 a2) (addCD b1 b2)
addCD a b =  fillMissing addCD a b

--figuring out neutrall addition element requires getting an example specimen
neutralAddCD :: (Operations a) => CDnum a ->  CDnum a
neutralAddCD (Base _) = (Base neutralAdd)
neutralAddCD num = (Pair (neutralAddCD num) (neutralAddCD num))

oppositeAddCD :: (Operations a) => CDnum a -> CDnum a
oppositeAddCD (Base a) = Base (oppositeAdd a)
oppositeAddCD (Pair a b) = Pair (oppositeAddCD a) (oppositeAddCD b)

subCD :: (Operations a) => CDnum a -> CDnum a -> CDnum a
subCD a b = addCD a (oppositeAddCD b)

mulCD :: (Operations a) => CDnum a -> CDnum a -> CDnum a
mulCD (Base a) (Base b) = Base (mul a b)
mulCD (Pair a1 b1) (Pair a2 b2) = Pair
  (subCD (mulCD a1 a2) (mulCD (conjugateCD b2) b1))
  (addCD (mulCD a1 b2) (mulCD (conjugateCD a2) b1))
mulCD a b  = fillMissing mulCD a b

--requires example specimen
neutralMulCD :: (Operations a) => CDnum a -> CDnum a
neutralMulCD (Base _) = Base neutralMul
neutralMulCD (Pair a b) = Pair (neutralMulCD a) (neutralAddCD b) --todo to A i B nie powinno być potrzebne. do modelowania zagnieżdżeń przydałby się IDric albo co

oppositeMulCD :: (Operations a) => CDnum a -> CDnum a
oppositeMulCD (Base a) = Base (oppositeMul a)
oppositeMulCD (Pair a b) = Pair
  (divCD (conjugateCD a) (addCD (sqNormCD a) (sqNormCD b)))
  (oppositeAddCD (divCD b (addCD (sqNormCD a) (sqNormCD b))))
--todo sqrNorm to powinno 'a' zwracać!!
--ale z wikipedii: a* `mul`  a = |a|^2 
sqNormCD :: (Operations a) => CDnum a -> CDnum a
sqNormCD a = (mulCD (conjugateCD a) a)


divCD :: (Operations a) => CDnum a -> CDnum a -> CDnum a
divCD a b = mulCD a (oppositeMulCD b)

conjugateCD :: (Operations a) => CDnum a -> CDnum a --sprzężenia
conjugateCD (Base a) = Base (conjugate a)
conjugateCD (Pair a b) = Pair (conjugateCD a) (oppositeAddCD b)


--TODO: 'a' tworzy unormowaną przestrzeń liniową
class  (Eq a) => Operations a where
  add :: a -> a -> a
  neutralAdd :: a
  oppositeAdd :: a -> a
  mul :: a -> a -> a
  neutralMul :: a
  oppositeMul :: a -> a
  conjugate :: a-> a --sprzężony
  sqr :: a -> a
  --norma :: a -> a

instance Operations Double where
  add = (+)
  neutralAdd = 0
  oppositeAdd a = -a
  mul = (*)
  neutralMul = 1
  oppositeMul a = 1/a
  conjugate a = a
  sqr = sqrt 
  ---norma a = a
    
  
