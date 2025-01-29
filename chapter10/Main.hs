{-# LANGUAGE TypeApplications #-}
module Main(main) where
import Data.IORef (newIORef, writeIORef, readIORef)
import Control.Exception (IOException, handle)
import Control.Monad (join, void, when)
import Data.Foldable (for_)
import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef)
import Data.List (isSuffixOf)
import System.Directory
  ( canonicalizePath
  , doesDirectoryExist
  , doesFileExist
  , listDirectory
  )
import qualified Data.Set as Set (empty, insert, member)
import Text.Printf (printf)

readWriteRef :: IO Int
readWriteRef = do
  myRef <- newIORef @Int 0
  writeIORef myRef 7
  refValue <- readIORef myRef
  pure refValue
-- IORef to traverse a directory tree
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
naiveTraversal :: FilePath -> (FilePath -> a) -> IO [a]
naiveTraversal rootPath action = do
  classification <- classifyFile rootPath
  case classification of
    FileTypeOther ->
      pure []
    FileTypeRegularFile ->
      pure $ [action rootPath]
    FileTypeDirectory -> do
      contents <- map (fixPath rootPath) <$> listDirectory rootPath
      results <- concat <$> getPaths contents
      pure results
  where
    fixPath parent fname = parent <> "/" <> fname
    getPaths = mapM (\path -> naiveTraversal path action)
traverseDirectory :: FilePath -> (FilePath -> a) -> IO [a]
traverseDirectory rootPath action = do
  visited <- newIORef @Set empty
  let visit path = do
        alreadyVisited <- Set.member path <$> readIORef visited
        if alreadyVisited
          then pure []
          else do
            writeIORef visited $ Set.insert path visited
            naiveTraversal path action
  visit rootPath
main :: IO ()
main = readWriteRef >>= print
