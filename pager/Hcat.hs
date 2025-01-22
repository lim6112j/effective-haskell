{-# LANGUAGE LambdaCase #-}
module Hcat(runHCat) where
import qualified System.Environment as Env
import Prelude hiding (FilePath)
getArgs :: IO [String]
getArgs = Env.getArgs
type FilePath = String
handleArgs :: IO (Either String FilePath)
handleArgs = getArgs >>= \case
    [arg] -> return $ Right arg
    [] -> return $ Left "no arguments"
    _ -> return $ Left "Usage: hcat <file>"

runHCat :: IO ()
runHCat = handleArgs >>= \case
  Left err ->
    putStrLn $ "Error Processing: " <> err
  Right file ->
    readFile file >>= putStrLn
