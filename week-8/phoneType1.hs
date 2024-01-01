-- Phone type

{-
Using the data keyword, define PhoneType type that has constructors for values WorkLandline, PrivateMobile, WorkMobile, and Other. 
Use the deriving keyword to derive instance for Show and Eq for it.

Using the type keyword, define two type synonyms for Integer: CountryCode and PhoneNo.

Then, using the record syntax (named fields), define Phone type for phone numbers that has only one value constructor with fields.
Use the deriving keyword to derive instances for Show and Eq for the Phone type.

Make a function makePhone that throws an error (using the error function) if CountryCode or PhoneNo is a negative integer
and otherwise creates a value of type Phone with the given values.
-}

data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Show, Eq)

-- type synonyms for Integer
type CountryCode = Integer
type PhoneNo = Integer

-- record syntax
data Phone = Phone { phoneType :: PhoneType 
                    , countryCode :: CountryCode 
                    , phoneNo :: PhoneNo
                    } deriving (Show, Eq)


countryCodeError = "Negative country code"
phoneNoError = "Negative phone number"

-- function
makePhone :: PhoneType -> CountryCode -> PhoneNo -> Phone
makePhone phoneType code num
    | code < 0 = error countryCodeError
    | num < 0 = error phoneNoError
    | otherwise = Phone phoneType code num 


