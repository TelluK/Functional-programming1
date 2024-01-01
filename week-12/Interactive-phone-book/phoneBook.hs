-- Phone book, from week 9
module PhoneBook
  ( emptyBook,
    PhoneBookEntry(..),
    PhoneBook(..),
    addEntry,
    findEntries,
    readPhone,
    PhoneType(..),
    CountryCode(..),
    PhoneNo(..),
    Phone(..)
  ) where

{-
for testing:  
readPhone "Other" "358" "123456" [40,49,358]

Phone {phoneType = Other, countryCode = CountryCode 358, phoneNo = PhoneNo 123456}

-}

emptyBook :: PhoneBook
emptyBook = []

data PhoneBookEntry = PhoneBookEntry { name :: String , phone :: Phone } deriving (Eq, Show)
type PhoneBook = [PhoneBookEntry]  -- type synonym

addEntry :: String -> String -> String -> String -> [Integer] -> PhoneBook -> PhoneBook
addEntry "" _ _ _ _ currentbook = currentbook 
addEntry name phonetype ccode phonenum ccodelist [] = 
  PhoneBookEntry { name=name , phone= (readPhone phonetype ccode phonenum ccodelist) } : emptyBook
addEntry name phonetype ccode phonenum ccodelist currentbook = 
  if filteredBook == []
    then PhoneBookEntry { name=name , phone= newPhone } : currentbook
    else if sameNameAndnumber == []
      then PhoneBookEntry { name=name , phone= newPhone } : currentbook
      else currentbook
      where filteredBook = findEntries name currentbook
            newPhone = (readPhone phonetype ccode phonenum ccodelist)
            sameNameAndnumber = filter (\item -> show (phoneNo (phone item)) == ( phonenum) ) filteredBook

-- Find a list of entries by a name
findEntries :: String -> PhoneBook -> PhoneBook
findEntries "" _ = emptyBook
findEntries _ [] = emptyBook
findEntries nameToFind phonebook = filter (\x -> name x == nameToFind) phonebook


-- Below is code from previous exercise "read phone"

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

data CountryCode = CountryCode Integer deriving (Eq)
instance Show CountryCode where
  show (CountryCode x) = "+" ++ (show x)

data PhoneNo = PhoneNo Integer deriving (Eq)
instance Show PhoneNo where
  show (PhoneNo number) = show number

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
                    } deriving (Eq)

-- show print form: <country code><space><phone number><space><phone type in parenthesis>
-- e.g. +358 123456789 (WorkLandline)
instance Show Phone where
  show (Phone phoneType countryCode phoneNumber) = (show countryCode) ++ " " ++ (show phoneNumber) ++" " ++"(" ++ (show phoneType) ++ ")"
