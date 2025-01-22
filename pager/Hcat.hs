{-# LANGUAGE LambdaCase #-}
module Hcat(runHCat, runHCat', runHCat'') where
import qualified System.Environment as Env
import Prelude hiding (FilePath)
import qualified Control.Exception as Exception
import qualified System.IO.Error as IOError
getArgs :: IO [String]
getArgs = Env.getArgs
type FilePath = String
handleArgs :: IO (Either String FilePath)
handleArgs = getArgs >>= \case
    [arg] -> return $ Right arg
    [] -> return $ Left "no arguments"
    _ -> return $ Left "Usage: hcat <file>"

eitherToErr :: Show a => Either a b -> IO b
eitherToErr (Right a) = return a
eitherToErr (Left e) = Exception.throwIO . IOError.userError $ show e

runHCat :: IO ()
runHCat = Exception.catch
  ( handleArgs >>= \case
  Left err ->
    putStrLn $ "Error Processing: " <> err
  Right file ->
    readFile file >>= putStrLn
  ) handleErr
  where
    handleErr :: Exception.IOException -> IO ()
    handleErr e = putStrLn $ "Error: " <> show e
runHCat' :: IO ()
runHCat' =
  withErrorHandling $
   handleArgs >>= \case
    Left err -> putStrLn $ "Error Processing: " <> err
    Right file -> readFile file >>= putStrLn
  where
    withErrorHandling :: IO () -> IO ()
    withErrorHandling io = Exception.catch io handleErr
    handleErr :: IOError -> IO ()
    handleErr e = putStrLn $ "Error: " <> show e
runHCat'' :: IO ()
runHCat'' =
  handleIOError $
     handleArgs
     >>= eitherToErr
     >>= readFile
     >>= putStrLn
  where
    handleIOError :: IO () -> IO ()
    handleIOError io = Exception.catch io handleErr
    handleErr :: IOError -> IO ()
    handleErr e = putStrLn $ "Error: " <> show e
