{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hcat(runHCat, runHCat', runHCat'', groupsOf, wordWrap, wordWrap') where
import qualified System.Environment as Env
import Prelude hiding (FilePath)
import qualified Control.Exception as Exception
import qualified System.IO.Error as IOError
import qualified Data.Text.IO as TextIO
import qualified Data.Text as Text
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
-- using Efficient Strings
runHCat''' :: IO ()
runHCat''' =
  handleIOError $
     handleArgs
     >>= eitherToErr
     >>= TextIO.readFile
     >>= TextIO.putStrLn
  where
    handleIOError :: IO () -> IO ()
    handleIOError io = Exception.catch io handleErr
    handleErr :: IOError -> IO ()
    handleErr e = putStrLn $ "Error: " <> show e
groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n xs =
  let (first, rest) = splitAt n xs
  in first : groupsOf n rest
wordWrap :: Int -> Text.Text -> [Text.Text]
wordWrap n  txt
  | Text.length txt <= n = [txt]
  | otherwise =
        let (line, rest) = Text.splitAt n txt
        in line : wordWrap n rest
wordWrap' :: Int -> Text.Text -> [Text.Text]
wordWrap' n txt
  | Text.length txt <= n = [txt]
  | otherwise =
        let (line, rest) = Text.splitAt n txt
            (modified, overflow) = softwrap line (Text.length line - 1)
        in modified : wordWrap n (overflow <> rest)
        where
                softwrap :: Text.Text -> Int -> (Text.Text, Text.Text)
                softwrap line idx
                  | idx <= 0 = (line, "")
                  | Text.index line idx == ' ' =
                      let (wrappedline, rest') = Text.splitAt idx line
                      in (wrappedline, Text.tail rest')
                  | otherwise = softwrap line (idx - 1)
