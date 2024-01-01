-- Distance functions 1

{-
Write a function distance1 :: String -> String -> Float that, given two strings s1 and s2, 
calculates their distance using the following formula:
count of how many of the characters in s1 do not appear in s2 + count of how many of the characters in s2 do not appear in s1 
then divide that by  length of s1 + length of s2. 
If both of the lists are empty, then the distance is 0. 
For example, the distance between “aaabc” and “aabdd” with this function is (1 + 2) / (5 + 5).

Write a function distance2 :: String -> String -> Float that, given two strings s1 and s2,
calculates their distance using the following formula:
count of characters in s1 that are other than any of ‘0’..‘9’ + count of characters in s2 that are other that any of ‘0’..‘9’
then divide that by length of s1 + length of s2. 
If both lists are empty, then the distance is 0. 
For example, the distance between “xy765” and “abc2311” with this function is (2 + 3) / (5 + 7).
-}

distance1 :: String -> String -> Float
distance1 [] [] = 0
distance1 str [] = fromIntegral ( length str + 0) / fromIntegral ( length str  + 0)
distance1 [] str = fromIntegral ( 0 + length str) / fromIntegral ( 0 + length str )
distance1 s1 s2 = (fromIntegral ( (helper1 s1 s2) + (helper1 s2 s1) )) / (fromIntegral ( length s1 + length s2))


-- counts how many different char is found
helper1 :: String -> String -> Int
helper1 [] [] = 0
helper1 [] _ = 0
helper1 (x:s1) s2 
  | elem x s2 = helper1 s1 s2 
  | otherwise = 1 + helper1 s1 s2 


digits = ['0'..'9']

countOfNotDigits :: String -> Int
countOfNotDigits [] = 0
countOfNotDigits (x:str)
  | elem x digits = countOfNotDigits str
  | otherwise = 1 + countOfNotDigits str


distance2 :: String -> String -> Float
distance2 [] [] = 0
distance2 s1 s2 = (fromIntegral ( (countOfNotDigits s1) + (countOfNotDigits s2) )) / (fromIntegral ( length s1 + length s2))

