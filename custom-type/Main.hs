module Main (main) where
data CustomerInfo = CustomerInfo String String Int Int deriving Show
customerGeorge :: CustomerInfo
customerGeorge = CustomerInfo "George" "Bird" 10 100
showCustomer :: CustomerInfo -> String
showCustomer (CustomerInfo first last count balance) =
  let fullName = first <> " " <> last
      name = "name: " <> fullName
      count' = "count: " <> (show count)
      balance' = "balance: " <> (show balance)
  in name <> ", " <> count' <> ", " <> balance'
applyDiscount :: CustomerInfo -> CustomerInfo
applyDiscount customer =
  case customer of
    (CustomerInfo "George" "Bird" count balance) ->
      CustomerInfo "George" "Bird" count (balance `div` 4)
    (CustomerInfo "Peter" "Pupper" count balance) ->
      CustomerInfo "Peter" "Pupper" count (balance `div` 2)
    otherCustomer ->
      otherCustomer
firstName :: CustomerInfo -> String
firstName (CustomerInfo first _ _ _) = first
lastName :: CustomerInfo -> String
lastName (CustomerInfo _ name _ _) = name
count :: CustomerInfo -> Int
count (CustomerInfo _ _ count _) = count
balance :: CustomerInfo -> Int
balance (CustomerInfo _ _ _ balance) = balance

data CustomerInfo' = CustomerInfoCon
  { firstName' :: String,
    lastName' :: String,
    count' :: Int,
    balance' :: Int
  }
  deriving Show
customerJane :: CustomerInfo'
customerJane = CustomerInfoCon {firstName' = "Jane", lastName' = "Ave", count' = 10, balance' = 100 }
customerFactory :: String -> String -> CustomerInfo'
customerFactory fname lname =
  CustomerInfoCon {firstName' = fname,
                  lastName' = lname,
                  balance' = 0,
                  count' = 0}
totalCount :: [CustomerInfo'] -> Int
totalCount = sum . map count'
main :: IO()
main = do
  print $ showCustomer customerGeorge
  print $ applyDiscount customerGeorge
  print customerJane
  print $ firstName' (customerFactory "Tom" "Boy")
  print $ totalCount [customerFactory "Tom" "Boy"]
