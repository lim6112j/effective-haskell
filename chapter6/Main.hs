{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Main(main) where
unique :: (a -> a -> Bool) -> [a] -> [a]
unique _ [] = []
unique f (x:xs) = x : unique f (filter (\y -> not (f x y)) xs)
-- signature for default instance definition
class Redacted a where
  redacted :: a -> String
  default redacted :: Show a => a -> String
  redacted = show
data UserName= UserName String
data Password= Password String

instance Show UserName where
  show (UserName name) = name
instance Redacted UserName where
  redacted (UserName name) = name
instance Redacted Password where
  redacted _ = "REDACTED"
-- type application
showLeftRight :: (Read a, Read b) => String -> Either a b
showLeftRight s
  | length s > 5 = Left (read s)
  | otherwise = Right (read s)
adheresToReadShowContract :: forall a. (Show a, Read a) => a -> Bool
adheresToReadShowContract val =
  let a = show . read @a . show $ val
      b = show val
  in a == b
-- specified and Infered type
convertViaInt :: forall {a} b. (Integral a, Num b) => a -> b
convertViaInt input = fromIntegral $ fromIntegral @_ @Integer input
convertToDouble :: Int -> Double
convertToDouble = convertViaInt @Double
-- wrapping Types with newtype
newtype MyEither a b = MyEither (Either a b)
  deriving (Show)
newtype MyEither' a b = MyEither' {getEither :: Either a b}
  deriving (Show)
-- using type classes with newtypes


main :: IO ()
main = do
  print "Hello, World!"
  print $ unique (==) [1, 2, 3, 4, 5, 5, 4, 3, 2, 1]
  print $ UserName "John"
  print $ redacted $ Password "1234"
