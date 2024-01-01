-- Read phone

{-
Implement the readPhone function as follows:
1. reads the phone type from phonetypestr.
2. reads the country code code from countrycodestr in the following way:
  a. if the code has a ‘+’ or “00” in the front, remove them
  b. read an integer out of the remaining string
  c. check that the code exists in a list of allowed country codes given as ccodelist
  d. call the function (that checks that the integer is >= 0) you created in task Phone type v2 
  with the integer to create the value for CountryCode.
3. reads the phone number from phonenostr by reading it as an integer and then calling the function you created in task Phone type v2.
-}

readPhone :: String -> String -> String -> [Integer] -> Phone
readPhone phonetypestr countrycodestr phonenostr ccodelist
  | (code `notElem` ccodelist) == True = error "Unknown country code"
  | otherwise = Phone phoneType countryCode phoneNo
  where codeX = checkCountryCodeStr countrycodestr
        code =  read codeX :: Integer
        phoneType = toPhoneType phonetypestr
        countryCode = toCountryCode code
        phoneNocheck = checkPhoneNoStr phonenostr
        checkedPhoneNoStr = read (checkPhoneNoStr phonenostr) :: Integer 
        phoneNo = toPhoneNo checkedPhoneNoStr


checkPhoneNoStr str 
  | null str == True  = error "Empty phone number"
  | head str == '-'  = error "Negative phone number"
  | otherwise = if all (\x -> x `elem` digitChars) str
                then str
                else error "Incorrect phone number"


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

toPhoneType str
  | null str == True    = error "Missing phone type"
  | length (filter (\x -> (show x) == str) phoneTypesInList) <= 0 = error "Incorrect phone type"
  | otherwise = read str :: PhoneType

data CountryCode = CountryCode Integer deriving (Show, Eq)
data PhoneNo = PhoneNo Integer deriving (Show, Eq)

countryCodeError = "Negative country code"
phoneNoError = "Negative phone number"

toCountryCode int 
  | int < 0 = error countryCodeError
  | otherwise = CountryCode int

toPhoneNo int
  | int < 0 = error phoneNoError
  |otherwise = PhoneNo int

-- record syntax
data Phone = Phone { phoneType :: PhoneType 
                    , countryCode :: CountryCode 
                    , phoneNo :: PhoneNo
                    } deriving (Show, Eq)

