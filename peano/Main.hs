module Main(main) where

data Peano = Z | S Peano deriving (Show)
toPeano :: Int -> Peano
toPeano 0 = Z
toPeano n = S (toPeano (n - 1))

fromPeano :: Peano -> Int
fromPeano Z = 0
fromPeano (S p) = succ (fromPeano p)

eqPeano :: Peano -> Peano -> Bool
eqPeano p p' =
  case (p, p') of
    (Z, Z) -> True
    (S p, S p') -> eqPeano p p'
    _ -> False
addPeano :: Peano -> Peano -> Peano
addPeano Z b = b
addPeano (S a) b = addPeano a (S b)
subPeano :: Peano -> Peano -> Peano
subPeano a Z = a
subPeano Z b = Z
subPeano (S a) (S b) = subPeano a (b)

aPeano :: Peano
aPeano = toPeano 5
bPeano :: Peano
bPeano = toPeano 5
cPeano :: Peano
cPeano = toPeano 6
aSub :: Peano
aSub = subPeano cPeano bPeano
main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  print $ eqPeano aPeano bPeano
  print $ eqPeano aPeano cPeano
  print $ subPeano aPeano bPeano
  print $ subPeano aPeano cPeano
  print $ subPeano cPeano bPeano
  print $ addPeano aPeano bPeano
