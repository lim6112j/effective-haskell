module Main(main) where
unique :: (a -> a -> Bool) -> [a] -> [a]
unique _ [] = []
unique f (x:xs) = x : unique f (filter (\y -> not (f x y)) xs)
-- signature for default instance definition
class Redacted a where
        redacted :: a -> String
data UserName= UserName String
--instance Show UserName where
--  show (UserName name) = name
-- instance Redacted UserName where
--  redacted (UserName name) = name
instance Show a => Redacted a where
  redacted = show
instance Redacted UserName where

main :: IO ()
main = do
  print "Hello, World!"
  print $ unique (==) [1, 2, 3, 4, 5, 5, 4, 3, 2, 1]
  print UserName "John"
