module Main(main) where
data StringParser = StringParser { runStringParser :: String -> (String, String) }
takeCharacters :: Int -> (String -> (String, String))
takeCharacters numCharacters = stringParser
        where stringParser :: String -> (String, String)
              stringParser  = \str -> splitAt numCharacters str
takeCharacters' :: Int -> StringParser
takeCharacters' numCharacters = StringParser stringParser
        where stringParser :: String -> (String, String)
              stringParser  = \str -> splitAt numCharacters str
takeCharacters'' :: Int -> StringParser
takeCharacters'' numcharacters = StringParser (\str -> splitAt numcharacters str)

getNextWord :: StringParser
getNextWord = StringParser $ \something ->
  case break (== ' ') something of
    (nextword, "") -> (nextword, "")
    (nextword, rest) -> (nextword, tail rest)

combineParsers :: StringParser -> StringParser -> StringParser
combineParsers firstParser secondParser = StringParser $ \str ->
  let (_firstPart, firstResult) = runStringParser firstParser str
  in runStringParser secondParser firstResult
getNextwordAfterTenLetters :: StringParser
getNextwordAfterTenLetters = combineParsers (takeCharacters' 10) getNextWord

parseString :: StringParser -> String -> String
parseString parser str = fst $ runStringParser parser str
secondWord :: StringParser
secondWord = combineParsers getNextWord getNextWord
thirdWord :: StringParser
thirdWord = combineParsers secondWord getNextWord
main :: IO ()
main = do
  print $ takeCharacters 3 "Hello, Haskell!"
  print $ runStringParser (takeCharacters' 3) "Hello, Haskell!"
  print $ runStringParser (takeCharacters'' 3) "Hello, Haskell!"
  print $ runStringParser getNextWord "Hello, Haskell!"
  print $ runStringParser getNextwordAfterTenLetters "Hello, Haskell!"
  print $ parseString getNextwordAfterTenLetters "Hello, Haskell!"
  print $ parseString thirdWord "hello world cool how are you"
