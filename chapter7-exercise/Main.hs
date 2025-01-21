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
-- commandline calculator
calculator :: IO ()
calculator = do
  putStrLn "Enter a number:"
  nums <- words <$> getLine
  putStrLn "Enter an operator:"
  operator <- getLine
  let result = case operator of
        "+" -> foldl1 (+) (map read nums)
        "-" -> foldl1 (-) (map read nums)
        "*" -> foldl1 (*) (map read nums)
        "/" -> foldl1 div (map read nums)
        _ -> error "Invalid operator"
  putStrLn $ "The result is: " ++ show result
-- word replacement utility
type Filepath = String
type Needle = String
type Replacement = String
replaceword :: Filepath -> Needle -> Replacement -> IO ()
replaceword file needle replacement = do
  content <- readFile file
  print content
  let newContent = unwords $ map (\word -> if word == needle then replacement else word) $ words content
  writeFile file newContent
  content' <- readFile file
  print content'
main :: IO ()
main =
  getIOString "Hello world"
  >>= putStrLn
  >> (sequenceIO $ map print [1..10])
  >>= print
  >> print "sequenceIO_ runing \n"
  >> sequenceIO_ (map print [1..10])
  >> calculator
  >> replaceword "./test.txt" "world!" "world!!!!!!!!!!!"
