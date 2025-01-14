module Main(main) where
-- reversing list
reverseList :: [a] -> [a]
reverseList = foldl (flip (:)) []
reverseList' :: [a] -> [a]
reverseList' = foldr (\x xs -> xs ++ [x]) []
-- zipWith' with foldl
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f a b = foldl (\acc (x, y) -> acc ++ [f x y]) [] $ rest
  where rest = zip' a b
        zip' :: [a] -> [b] -> [(a, b)]
        zip' [] _ = []
        zip' _ [] = []
        zip' (x:xs) (y:ys) = (x, y) : zip' xs ys
concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' f = foldr (\x acc -> f x ++ acc) []
concatMap'' :: (a -> [b]) -> [a] -> [b]
concatMap'' f = foldl (\acc x -> acc ++ f x) []
func1 :: Num c => (a -> b) -> (b -> c -> c) -> [a] -> c
func1 f g = foldr g 0 . map f
func2 :: Num c => (a -> c) -> (c -> c -> c) -> [a] -> c
func2 f g = foldr (g . f) 0
func1' f g = foldr g 0 . map f
func2' f g = foldr (g . f) 0

foldrr :: (a -> b -> b) -> b -> [a] -> b
foldrr _ z [] = z
foldrr f z (x:xs) = f x (foldrr f z xs)

main :: IO ()
main = do
  print $ reverseList [1, 2, 3, 4, 5]
  print $ zipWith (+) [1, 2, 3] [4, 5, 6]
  print $ zip [1, 2, 3] [4, 5, 6]
  print $ zipWith' (,) [1, 2, 3] [4, 5, 6]
  print $ concatMap (\x -> [x, x]) [1, 2, 3]
  print $ take 10 $ concatMap' (\x -> [x, x]) [1..]
  print $ func1 (/2) (+) [1, 2, 3]
  print $ func2 (/2) (+) [1, 2, 3]
  print $ func1' (/2) (+) [1, 2, 3]
  print $ func2' (/2) (+) [1, 2, 3]
  print $ foldrr (+) 0 [1, 2, 3]
