module Main(main) where
pointful:: [Int] -> Int -> Int
pointful xs n = foldr (+) 0 xs * n
etaReduced :: [Int] -> Int -> Int
etaReduced xs = (*) (foldr (+) 0 xs)
pointfree :: [Int] -> Int -> Int
pointfree = (*) . foldr (+) 0
sumBiggest :: [[Int]] -> String
sumBiggest = undefined
showBiggest = let biggestInfo = sumBiggest [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
              in print $ "The biggest sum is: " <> biggestInfo
main :: IO ()
main = do
  print "Hello, World!"
