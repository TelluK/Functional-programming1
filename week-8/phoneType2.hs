-- Phone type v2 


data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Show, Eq)

countryCodeError = "Negative country code"
phoneNoError = "Negative phone number"

data CountryCode = CountryCode Integer deriving (Show, Eq)
data PhoneNo = PhoneNo Integer deriving (Show, Eq)

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



