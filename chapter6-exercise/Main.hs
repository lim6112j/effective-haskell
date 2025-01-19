{-# LANGUAGE DefaultSignatures #-}
module Main(main) where
import Prelude hiding (null)
class Nullable a where
  isNull :: a -> Bool
  default isNull :: Eq a => a -> Bool
  isNull = (== null)
  null :: a
instance Nullable a => Nullable (Maybe a) where
  isNull Nothing = True
  isNull (Just a) = isNull a
  null = Nothing
newtype Tuple a b = Tuple (a, b)
instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a) (Tuple b) = (==) a b
instance (Nullable a, Nullable b)  => Nullable (Tuple a b) where
  isNull (Tuple (a,b))
    | isNull a && isNull b = True
    | otherwise = False
  null = Tuple (null, null)
instance Eq a => Nullable [a] where
  null = []
main :: IO()
main = do
  print "hello world"
