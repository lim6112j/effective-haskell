module Main(main) where
import Text.Read (readMaybe)
newtype Function a b = Function { runFunction :: a -> b }
instance Functor (Function a) where
  fmap f (Function g) = Function $ f . g
instance Applicative (Function a) where
  pure a = Function $ const a
  Function f <*> Function g = Function $ \a -> f a (g a)
data List a = List a (List a) | Empty
instance Functor List where
  fmap _ Empty = Empty
  fmap f (List a rest) = List (f a) (fmap f rest)
instance Applicative List where
        pure a = List a Empty
        Empty <*> _ = Empty
        _ <*> Empty = Empty
        List f restF <*> List a restA = List (f a) (restF <*> restA)
instance Semigroup (List a) where
  Empty <> a = a
  a <> Empty = a
  List a rest <> b = List a (rest <> b)
instance Monoid (List a) where
        mempty = Empty
        mappend = (<>)
instance Monad List where
  return a = List a Empty
  Empty >>= _ = Empty
  List a rest >>= f = f a `mappend` (rest >>= f)

-- return half of a value if it's even, otherwise Nothing
half :: Int -> Maybe Int
half n
  | even n = Just (n `div` 2)
  | otherwise = Nothing
-- takes a boundary. Returns Just the value if it's within the range.
-- and Nothing otherwise
bound :: (Int, Int) -> Int -> Maybe Int
bound (min, max) n =
  if (n >= min) && (n <= max)
  then Just n
  else Nothing
-- Every function returns a Just value
  
main :: IO ()
main = putStrLn "Hello, Haskell!"
