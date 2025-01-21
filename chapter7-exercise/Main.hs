{-# LANGUAGE ScopedTypeVariables #-}

module Main(main) where

-- write a function that returns a value of type IO String , what happens if you try to use >>= with that. what if you want to print the string
getIOString :: String -> IO String
getIOString = return
uncoverIO :: IO (IO a) -> IO a
uncoverIO = (>>=(>>= return))
-- write a function returns [IO a] and second function with the type [IO a] -> IO [a] , when might you use a function like that
listIO :: [a] -> [IO a]
listIO = map return
sequenceIO :: [IO a] -> IO [a]
sequenceIO [] = return []
sequenceIO (x:xs) = rest >>= \rest' -> x >>= \x' -> return  $ x':rest'
  where rest = sequenceIO xs
sequenceIO' :: [IO a] -> IO [a]
sequenceIO' [] = return []
sequenceIO' (x:xs) = do
  x' <- x
  rest' <- sequenceIO' xs
  return $ x':rest'
sequenceIO_ :: [IO a] -> IO ()
sequenceIO_ [] = return ()
sequenceIO_ actions = sequenceIO actions >> return ()
main :: IO ()
main =
  getIOString "Hello world"
  >>= putStrLn
  >> (sequenceIO $ map print [1..10])
  >>= print
  >> print "sequenceIO_ runing \n"
  >> sequenceIO_ (map print [1..10])
