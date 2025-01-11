{-# LANGUAGE RecordWildCards #-}
module Main(main) where
data CustomerInfo = CustomerInfo
  {
    firstName :: String,
    lastName :: String,
    widgetCount :: Int,
    balance :: Int
  }
showCustomer :: CustomerInfo -> String
showCustomer CustomerInfo{..} = "Customer " ++ firstName ++ " " ++ lastName ++ " has " ++ show widgetCount ++ " widgets and a balance of " ++ show balance
customerGeorge = CustomerInfo {firstName = "George", lastName = "Smith", widgetCount = 10, balance = 100}
customerFactory :: String -> String -> CustomerInfo
customerFactory firstName lastName =
  let widgetCount = 10
      balance = 100
  in CustomerInfo {..}

main :: IO()
main = do
  print $ showCustomer CustomerInfo {firstName = "John", lastName = "Doe", widgetCount = 10, balance = 100}
  print $ showCustomer $ customerFactory "Johny" "Doew"
