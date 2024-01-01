-- Only digits

{-
This task is here for you to practice basic recursion.

Write a function onlyDigits that, given a string, checks whether the string contains 
only digits or not. Empty string should return false.

-}

-- digits are from 0 to 9

onlyDigits :: String -> Bool
onlyDigits "" = False
onlyDigits (x:xs)
  | (x >= '0') && (x <= '9') && xs == "" = True
  | (x >= '0') && (x <= '9') = onlyDigits xs
  | otherwise = False

