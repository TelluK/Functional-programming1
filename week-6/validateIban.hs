-- Validate IBAN

{-
Write a function validate :: String -> Bool that, 
given a string validates the string as a Finnish IBAN code.

For details:
https://en.wikipedia.org/wiki/International_Bank_Account_Number#Validating_the_IBAN

You will also need the following information:
Length of a Finnish IBAN code is 18.
Finnish IBAN begins with the country code FI.
All the characters after the country code are digits.
You can assume that the input is without whitespaces.
-}

digits = ['0'..'9']

-- 'F' = 15, 'I' = 18
moveAndReplace iban = drop 4 iban ++ "15" ++ "18" ++ (drop 2(take 4 iban))


validate :: String -> Bool
validate [] = False
validate string
  | length string /= 18 = False
validate (f:s:rest)
  | f /= 'F' && s /= 'I' = False
  | length (filter (\x -> elem x digits) rest) /= 16 = False
  | read (moveAndReplace (f:s:rest)) `mod` 97 == 1 = True
  | otherwise = False
