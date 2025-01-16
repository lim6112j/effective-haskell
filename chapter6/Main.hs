{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}

module Main(main) where
import Data.Kind
import qualified Data.List.NonEmpty as NonEmpty
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
newtype Product = Product {getProduct :: Int} deriving (Show)
instance Semigroup Product where
        Product a <> Product b = Product (a * b)
instance Monoid Product where
        mempty = Product 1
newtype Sum = Sum {getSum :: Int}
instance Semigroup Sum where
  Sum a <> Sum b = Sum (a + b)
instance Monoid Sum where
  mempty = Sum 0
-- higher kinded types and polymorphism
toCSV ::
  forall (t :: Type -> Type) (a :: Type)
  . (Foldable t, Show a)
  => t a -> String
toCSV =
  let addField s a =
        s <> "," <> show a
      dropLeadingComma s =
        case s of
          ',':s' -> s'
          _ -> s
  in dropLeadingComma . foldl addField ""
csvThings :: String
csvThings =
  let plainList = toCSV [1, 2, 3]
      nonEmptyList = toCSV $ 1 NonEmpty.:| [2, 3]
  in unlines [plainList, nonEmptyList]
-- Using Higher kinded types with your own types and classes
class Select (f :: Type -> Type) where
  empty :: f a
  pick :: f a -> f a -> f a
instance Select Maybe where
  empty = Nothing
  pick Nothing a = a
  pick a _ = a
instance Select [] where
  empty = []
  pick = (<>)
-- deriving instance
data Customer = Customer
  { name :: String
  , mail :: String
  , email :: String
  }
instance Eq Customer where
  (==)
    (Customer name mail email)
    (Customer name' mail' email') =
    name == name' &&
    mail == mail' &&
    email == email'
instance Ord Customer where
  compare
    (Customer name mail email)
    (Customer name' mail' email') =
    compare name name' <>
    compare mail mail' <>
    compare email email'
instance Show Customer where
  show (Customer name mail email) =
    "Customer " <> show name <> show mail <> show email
newtype Name = Name String deriving (Show, Eq, Ord)
data Customer' = Customer'
  { name' :: Name
  , mail' :: String
  , email' :: String
  } deriving (Eq, Show, Ord)
-- deriving more things

main :: IO ()
main = do
  print "Hello, World!"
  print $ unique (==) [1, 2, 3, 4, 5, 5, 4, 3, 2, 1]
  print $ UserName "John"
  print $ redacted $ Password "1234"
  print $ getProduct $ Product 2 <> Product 3
  print $ getSum $ Sum 2 <> Sum 3
  print $ toCSV "hello"
  print $ toCSV [1, 2, 3]
  print csvThings
  print $ toCSV @Maybe @Int Nothing
  print $ toCSV @Maybe @Int $ Just 1
  print $ toCSV @(Either Int) @Int $ Right 3
  print $ pick [1,2] [3,4]
  print $ foldl1 pick [Nothing, Nothing, Just "first", Nothing, Just "second"]
