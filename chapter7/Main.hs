module Main(main) where
import System.Environment (getArgs)
import Text.Read (readMaybe)
sumArgs :: [String] -> Maybe Int
sumArgs args =
  let intArgs = mapM readMaybe args
  in fmap sum intArgs
lazyIODemo :: IO()
lazyIODemo =
  let sayHello :: IO()
      sayHello = putStrLn "Hello"
      raiseMathError :: IO Int
      raiseMathError = putStrLn "I'm part of raiseMathError"
        >> return (1 `div` 0)
  in sayHello
  >> raiseMathError
  >> sayHello
makeAndReadFile :: Int -> IO String
makeAndReadFile n =
  let fname = "/tmp/test/" <> show n
  in writeFile fname fname >> readFile fname
makeAndShow :: Int -> IO ()
makeAndShow n =
  makeAndReadFile n >>= putStrLn
safe :: IO ()
safe =
  mapM makeAndShow [1..10]
safe' :: IO ()
safe' = foldl (\io n -> io >> makeAndShow n) (return ()) [1..10]
safe'' :: IO ()
safe'' = mapM_ makeAndShow [1..10]
main :: IO()
main =
  getArgs >>= \args -> return (sumArgs args) >>= print >> lazyIODemo
