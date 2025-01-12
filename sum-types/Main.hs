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
george :: Person
george = Customer
  { name = "Peter P. Porter"
  , balance = 100
  }

porter :: Person
porter = Employee
  { name = "Peter"
  , managerName = "Remi"
  , salary = 10
  }

data CustomerInfo = CustomerInfo
  { customerName :: String
  , customerBalance :: Int
  }
data EmployeeInfo = EmployeeInfo
  { employeeName :: String
  , employeeManagerName :: String
  , employeeBalance :: Int
  }
data Person'
  = Customer' CustomerInfo
  | Employee' EmployeeInfo

getPersonName :: Person' -> String
getPersonName person =
  case person of
    Customer' info -> customerName info
    Employee' info -> employeeName info
getPersonManagerName :: Person' -> String
getPersonManagerName person =
  case person of
    Customer' _ -> "No manager"
    Employee' info -> employeeManagerName info
data MaybeString = NoString | SomeString String
getPersonManager :: Person' -> MaybeString
getPersonManager person =
  case person of
    Customer' _ -> NoString
    Employee' info -> SomeString (employeeManagerName info)
data MaybeInt = NoInt | SomeInt Int
getPersonBalance :: Person' -> MaybeInt
getPersonBalance person =
  case person of
    Customer' info -> SomeInt (customerBalance info)
    Employee' info -> SomeInt (employeeBalance info)
getPersonsalary :: Person' -> MaybeInt
getPersonsalary person =
  case person of
    Customer' _ -> NoInt
    Employee' info -> SomeInt (employeeBalance info)

-- polymorphic data type
data MyMaybe a = MyNothing | MyJust a
-- either to maybe
eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe eitherValue =
  case eitherValue of
    Left _ -> Nothing
    Right value -> Just value
handleMissingRight :: Either String (Maybe a) -> Either String a
handleMissingRight eitherValue =
  case eitherValue of
    Left message -> Left message
    Right Nothing -> Left "Missing value"
    Right (Just value) -> Right value

main :: IO()
main = do
  print $ show stringAndNumber
  print $ confirmContact (Email "hello")
