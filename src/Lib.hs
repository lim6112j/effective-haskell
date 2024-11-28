{-# LANGUAGE TypeApplications #-}
module Lib
    ( someFunc
    ) where
import Data.IORef
readWriteRef :: IO Int
readWriteRef = do
  myRef <- newIORef @Int 0
  writeIORef myRef 7
  readIORef myRef

someFunc :: IO ()
someFunc = readWriteRef >>= print
