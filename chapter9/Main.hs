module Main(main) where
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
main :: IO ()
main = putStrLn "Hello, Haskell!"
