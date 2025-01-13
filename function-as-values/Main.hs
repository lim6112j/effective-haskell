module Main(main) where
data StringParser = StringParser { runStringParser :: String -> (String, String) }
main :: IO ()
main = do
  print "Hello, World!"
  print "Goodbye, World!"
