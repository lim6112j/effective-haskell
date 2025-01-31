{-# language RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module Metrics(main) where
import qualified Data.Map.Strict as Map
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Time.Clock
  ( diffUTCTime
  , getCurrentTime
  , nominalDiffTimeToSeconds)
import Text.Printf (printf)
import Data.Foldable (for_)
import qualified Data.Set as Set
import Control.Exception (IOException, handle)
import System.Directory
  ( canonicalizePath
  , doesDirectoryExist
  , doesFileExist
  , listDirectory
  )
import Control.Monad (unless)
import Data.List (isSuffixOf)
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

-- refactgor metrics
newtype Metrics = Metrics { appMetricsStore :: IORef AppMetrics}
newMetrics :: IO Metrics
newMetrics = Metrics <$> newIORef AppMetrics
  { successCount = 0
  , failureCount = 0
  , callDuration = Map.empty}
tickSuccess :: Metrics -> IO ()
tickSuccess (Metrics metricsRef) = modifyIORef metricsRef $ \m ->
  m { successCount = 1 + successCount m }
tickFailure :: Metrics -> IO ()
tickFailure (Metrics metricsRef) = modifyIORef metricsRef $ \m ->
  m { failureCount = 1 + failureCount m }
timeFunction :: Metrics -> String -> IO a -> IO a
timeFunction (Metrics metrics) actionName action = do
  startTime <- getCurrentTime
  result <- action
  endTime <- getCurrentTime
  modifyIORef metrics $ \oldMetrics ->
    let
      oldDurationValue =
        fromMaybe 0 $ Map.lookup actionName (callDuration oldMetrics)
      runDuration =
        floor . nominalDiffTimeToSeconds $
        diffUTCTime endTime startTime
      newDurationValue = oldDurationValue + runDuration
    in oldMetrics {
      callDuration = Map.insert actionName newDurationValue $
        callDuration oldMetrics
  }
  return result
displayMetrics :: Metrics -> IO ()
displayMetrics (Metrics metricsStore) = do
  AppMetrics{..} <- readIORef metricsStore
  putStrLn $ "success: " <> show successCount
  putStrLn $ "failure: " <> show failureCount
  for_ (Map.toList callDuration) $ \(functionName, timing) ->
    putStrLn $ printf "Time spent in \"%s\" %d" functionName timing
dropSuffix :: String -> String -> String
dropSuffix suffix s
  | suffix `isSuffixOf` s =
    take (length s - length suffix) s
  | otherwise = s
data FileType
  = FileTypeDirectory
  | FileTypeRegularFile
  | FileTypeOther
classifyFile :: FilePath -> IO FileType
classifyFile fname = do
  isDirectory <- doesDirectoryExist fname
  isFile <- doesFileExist fname
  pure $ case (isDirectory, isFile) of
    (True, False) -> FileTypeDirectory
    (False, True) -> FileTypeRegularFile
    _otherwise -> FileTypeOther
traverseDirectory :: Metrics -> FilePath -> (FilePath -> IO ()) -> IO ()
traverseDirectory metrics rootPath action = do
  seenRef <- newIORef Set.empty
  let
    haveSeenDirectory canonicalPath =
      Set.member canonicalPath <$> readIORef seenRef
    addDirectoryToSeen canonicalPath =
      modifyIORef seenRef $ Set.insert canonicalPath
    handler ex = print ex >> tickFailure metrics
    traverseSubDirectory subDirPath =
      timeFunction metrics "traverseSubDirectory" $ do
        contents <- listDirectory subDirPath
        for_ contents $ \file' ->
          handle @IOException handler $ do
          let file = subDirPath <> "/" <> file'
          canonicalPath <- canonicalizePath file
          classification <- classifyFile canonicalPath
          result <- case classification of
            FileTypeOther -> pure ()
            FileTypeRegularFile -> action file
            FileTypeDirectory -> do
              alreadyProcessed <- haveSeenDirectory file
              unless alreadyProcessed $ do
                addDirectoryToSeen file
                traverseSubDirectory file
          tickSuccess metrics
          pure result
  traverseSubDirectory (dropSuffix "/" rootPath)
main :: IO ()
main = do
    putStrLn "Hello, World!"
