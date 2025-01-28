{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main(main) where
import Text.Read (readMaybe)
import Data.Char
import Data.String
newtype Function a b = Function { runFunction :: a -> b }
instance Functor (Function a) where
  fmap f (Function g) = Function $ f . g
instance Applicative (Function a) where
  pure a = Function $ const a
  Function f <*> Function g = Function $ \a -> f a (g a)
data List a = List a (List a) | Empty
type StringL = List Char
fromList :: List a -> [a]
fromList Empty = []
fromList (List a rest) = a : fromList rest
toList :: [a] -> List a
toList = foldr List Empty
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
instance Show a => Show (List a) where
  show = show . fromList
instance IsString (List Char) where
  fromString = toList

replicateL :: Int -> a -> List a
replicateL 0 _ = Empty
replicateL n a = List a (replicateL (n - 1) a)
wordsL :: StringL -> List StringL
wordsL = toList . fmap toList . words . fromList
unwordsL :: List StringL -> StringL
unwordsL = toList . unwords . fromList . fmap fromList
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
-- functor laws
data Outlaw a = Outlaw Int a deriving (Eq, Show)
instance Functor Outlaw where
  fmap f (Outlaw n a) = Outlaw (n + 1) (f a)
bang :: String -> String
bang = (<> "!")
upcase = map toUpper
billyTheKid = Outlaw 0 "bank robber"


main :: IO ()
main = do
  let ll = toList (map return [1..5]) :: List (List Int)
  print ll
  let lll = ll >>= id
  print lll
  let w = pure "hello haskell" >>= wordsL
  print w
  let ww = pure "hello haskell" >>= wordsL >>= replicateL 3
  print ww
  let www = pure "hello haskell" >>= wordsL >>= replicateL 3 >>= id
  print www
  let u = unwordsL $ pure "hello haskell" >>= wordsL
  print u
  let uu = unwordsL $ pure "hello haskell" >>= wordsL >>= const Empty
  print uu
  let testIdentityLaw = fmap id billyTheKid == id billyTheKid
  print testIdentityLaw
  let testCompositionLaw = fmap (bang . upcase) billyTheKid == (fmap bang . fmap upcase) billyTheKid
  print testCompositionLaw
