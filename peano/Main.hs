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
subPeano Z _ = Z
subPeano (S a) (S b) = subPeano a (b)

aPeano :: Peano
aPeano = toPeano 5
bPeano :: Peano
bPeano = toPeano 5
cPeano :: Peano
cPeano = toPeano 6
aSub :: Peano
aSub = subPeano cPeano bPeano

-- inductively defined list
data List a = Empty | Cons a (List a) deriving (Show)
toList :: [a] -> List a
toList [] = Empty
toList (x:xs) = Cons x (toList xs)

toList' :: [a] -> List a
toList' = foldr Cons Empty
fromList :: List a -> [a]
fromList Empty = []
fromList (Cons x xs) = x : fromList xs

fromList' :: List a -> [a]
fromList' = listFoldr (:) []
listFoldr :: (a -> b -> b) -> b -> List a -> b
listFoldr _ b Empty = b
listFoldr f b (Cons x xs) =
  f x $ listFoldr f b xs

fromListl :: List a -> [a]
fromListl = listFoldl appendElement []
appendElement :: [a] -> a -> [a]
appendElement b a = b ++ [a]
listFoldl :: (b -> a -> b) -> b -> List a -> b
listFoldl _ b Empty = b
listFoldl f b (Cons x xs) =
  let b' = b `f` x
  in listFoldl f b' xs

listHead :: List a -> Maybe a
listHead l =
  case l of
    (Cons x _) -> Just x
    _ -> Nothing
listTail :: List a -> List a
listTail l =
  case l of
    Empty -> Empty
    Cons _ xs -> xs
listReverse :: List a -> List a
listReverse l =
  case fromList l of
    [] -> Empty
    (x:xs) -> toList $ fromList (listReverse $ toList xs) ++ [x]
listReverse' :: List a -> List a
listReverse' = listFoldl (flip Cons) Empty
listMap :: (a -> b) -> List a -> List b
listMap _ Empty = Empty
listMap f (Cons x xs) =
  Cons (f x) (listMap f xs)
aList :: List Int
aList = toList [1, 2, 3, 4, 5]

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  print $ eqPeano aPeano bPeano
  print $ eqPeano aPeano cPeano
  print $ subPeano aPeano bPeano
  print $ subPeano aPeano cPeano
  print $ subPeano cPeano bPeano
  print $ addPeano aPeano bPeano
  print $ fromList aList
  print $ fromListl aList
  print $ listReverse aList
  print $ listMap (* 2) aList
  print $ listReverse' aList
