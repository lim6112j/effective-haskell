module Main(main) where
data PreferedContactMethod = Email String | TextMessage String | Mail String String String Int
confirmContact :: PreferedContactMethod -> String
confirmContact contact =
        case contact of
        Email email -> "Emailing " ++ email
        TextMessage number -> "Texting " ++ number
        Mail address city state zip -> "Mailing " ++ address ++ " " ++ city ++ " " ++ state ++ " " ++ show zip
data StringOrNumber = S String | N Int deriving (Show)
stringAndNumber :: [StringOrNumber]
stringAndNumber = [S "one", N 2, S "three", N 4]

data Person = Customer
  {name :: String, balance :: Int}
  | Employee {name :: String, managerName :: String, salary :: Int}

main :: IO()
main = do
  print $ show stringAndNumber
  print $ confirmContact (Email "hello")
