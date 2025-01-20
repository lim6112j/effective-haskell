module Main(main) where
import System.Environment (getArgs)
import Text.Read (readMaybe)
sumArgs :: [String] -> Maybe Int
sumArgs args =
  let intArgs = mapM readMaybe args
  in fmap sum intArgs

main :: IO()
main =
  getArgs >>= \args -> return (sumArgs args) >>= print
