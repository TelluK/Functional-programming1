-- Phone type v3

{-
test with:
readPhone "Other" "358" "123456" [40,49,358]


TODO:
[x] toCountryCode :: Integer -> CountryCode
[x] toPhoneNo :: Integer -> PhoneNo
[x] fromPhoneNo :: PhoneNo -> Integer
[x] readPhoneType :: String -> Maybe PhoneType
[x] readCountryCode :: String -> [Integer] -> Maybe CountryCode
[x] readPhoneNo :: String -> PhoneNo
[x] readPhone :: String -> String -> String -> [Integer] -> Phone

-}

readPhone :: String -> String -> String -> [Integer] -> Phone
readPhone phonetypestr countrycodestr phonenostr ccodelist = Phone phoneType countryCode phoneNo
  where phoneType = readPhoneType phonetypestr  -- returns Maybe PhoneType
        countryCode = readCountryCode countrycodestr ccodelist  -- returns Maybe CountryCode
        phoneNo = readPhoneNo phonenostr  -- returns PhoneNo

fromPhoneNo :: PhoneNo -> Integer
fromPhoneNo no = read (checkPhoneNoStr (show no)) :: Integer

readPhoneNo :: String -> PhoneNo
readPhoneNo str = toPhoneNo checkedPhoneNoStr
  where checkedPhoneNoStr = read (checkPhoneNoStr str) :: Integer

checkPhoneNoStr str 
  | null str == True  = error "Empty phone number"
  | head str == '-'  = error "Negative phone number"
  | otherwise = if all (\x -> x `elem` digitChars) str
                then str
                else error "Incorrect phone number"


readCountryCode :: String -> [Integer] -> Maybe CountryCode
readCountryCode "" list = Nothing 
readCountryCode str ccodelist
  | (code `notElem` ccodelist) == True = error "Unknown country code"
  | otherwise = Just (toCountryCode code)
  where code =  read codeX :: Integer
        codeX = checkCountryCodeStr str

-- remove '+' or '00' in the front of string
-- then check if all rest elements are digits, if not throw error 
checkCountryCodeStr str 
  | null str == True  = error "Empty country code"
  | head str == '+' = checkCountryCodeStr ( tail str)
  | head str == '0' = checkCountryCodeStr ( tail str)
  | otherwise = if all (\x -> x `elem` digitChars) str
                then str
                else error "Incorrect country code"

digitChars = ['0'.. '9']

data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Show, Eq, Read, Enum)
phoneTypesInList = [ WorkLandline .. Other]

readPhoneType :: String -> Maybe PhoneType
readPhoneType str
  | null str == True    = Nothing
  | length (filter (\x -> (show x) == str) phoneTypesInList) <= 0 = Nothing
  | otherwise = Just (read str :: PhoneType)

newtype CountryCode = CountryCode Integer deriving (Eq)
instance Show CountryCode where
  show (CountryCode x) = "+" ++ (show x)

newtype PhoneNo = PhoneNo Integer deriving (Eq)
instance Show PhoneNo where
  show (PhoneNo number) = show number

countryCodeError = "Negative country code"
phoneNoError = "Negative phone number"

toCountryCode :: Integer -> CountryCode
toCountryCode int 
  | int < 0 = error countryCodeError
  | otherwise = CountryCode int

toPhoneNo :: Integer -> PhoneNo
toPhoneNo int
  | int < 0 = error phoneNoError
  | otherwise = PhoneNo int

-- record syntax
data Phone = Phone { phoneType :: Maybe PhoneType
                    , countryCode :: Maybe CountryCode
                    , phoneNo :: PhoneNo
                    } deriving (Eq)


-- show print form: <country code><space><phone number><space><phone type in parenthesis>
-- e.g. +358 123456789 (WorkLandline)
instance Show Phone where
  show (Phone (Just a) (Just b) phoneNumber) = (show b) ++ " " ++ (show phoneNumber) ++" " ++"(" ++ (show a) ++ ")"
  show (Phone Nothing (Just b) phoneNumber) = (show b) ++ " " ++ (show phoneNumber) 
  show (Phone (Just a) Nothing phoneNumber) = (show phoneNumber) ++" " ++"(" ++ (show a) ++ ")"
  show (Phone Nothing Nothing phoneNumber) = (show phoneNumber)

