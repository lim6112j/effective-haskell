module Metrics(main) where
import qualified Data.Map.Strict as Map
import Data.IORef
data AppMetrics = AppMetrics
  { successCount :: Int
  , failureCount :: Int
  , callDuration :: Map.Map String Int
  } deriving (Eq, Show)
metrics :: IO (IORef AppMetrics)
metrics = newIORef AppMetrics
  { successCount = 0
  , failureCount = 0
  , callDuration = Map.empty}
printMetrics :: IO ()
printMetrics =
  metrics >>= readIORef >>= print
incrementSuccess :: IO ()
incrementSuccess =
  metrics >>= flip modifyIORef incrementSuccess
  where
    incrementSuccess m =
      m { successCount = 1 + successCount m }
successfullyPrintHello :: IO ()
successfullyPrintHello = do
  print "Hello"
  incrementSuccess
printHelloAndMetrics :: IO ()
printHelloAndMetrics = do
  successfullyPrintHello
  printMetrics
main :: IO ()
main = do
    putStrLn "Hello, World!"
