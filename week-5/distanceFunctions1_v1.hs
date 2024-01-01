-- Distance functions 1

{-
Write a function distance1 :: String -> String -> Float that, given two strings s1 and s2, 
calculates their distance using the following formula:
( (count of how many of the characters in s1 do not appear in s2 + (count of how many of the characters in s2 do not appear in s1) ) / ( (length of s1) + (length of s2) ). 
If both of the lists are empty, then the distance is 0. 
For example, the distance between “aaabc” and “aabdd” with this function is (1 + 2) / (5 + 5)  = 0.3.

Write a function distance2 :: String -> String -> Float that, given two strings s1 and s2,
calculates their distance using the following formula:
 ( (count of characters in s1 that are other than any of ‘0’..‘9’) + (count of characters in s2 that are other that any of ‘0’..‘9’) ) / ( (length of s1) + (length of s2) ). 
If both lists are empty, then the distance is 0. 
For example, the distance between “xy765” and “abc2311” with this function is (2 + 3) / (5 + 7).

-}

distance1 :: String -> String -> Float
distance1 [] [] = 0
distance1 str [] = fromIntegral ( length str + 0) / fromIntegral ( length str  + 0)    -- 1.0
distance1 [] str = fromIntegral ( 0 + length str) / fromIntegral ( 0 + length str )    -- 1.0
distance1 s1 s2 = fromIntegral ( helper1 s1 s2 + helper1 s2 s1 ) / fromIntegral ( length s1 + length s2)


-- counts how many different char is found
helper1 :: String -> String -> Int
helper1 [] [] = 0
helper1 _ [] = 0
helper1 [] _ = 0
helper1 (x:s1) s2 
  | elem x s2 = helper1 s1 s2 
  | otherwise = 1 + helper1 s1 s2 

helper2 [] [] = 0
helper2 _ [] = 0
helper2 [] _ = 0
helper2 s1 (x:s2 )
  | elem x s1 = helper2 s1 s2 
  | otherwise = 1 + helper2 s1 s2 


distance2 :: String -> String -> Float
distance2 [] [] = 0
distance2 s1 s2 = 0


{-

distance1 :: String -> String -> Float
distance1 [] [] = 0
distance1 str [] = fromIntegral ( length str + 0) / fromIntegral ( length str  + 0)    -- 1.0
distance1 [] str = fromIntegral ( 0 + length str) / fromIntegral ( 0 + length str )    -- 1.0
-- distance1 s1 s2 = fromIntegral ( helper1 s1 s2 + helper2 s1 s2 ) / fromIntegral ( length s1 + length s2)
distance1 s1 s2 = fromIntegral ( helper1 s1 s2 + helper1 s2 s1 ) / fromIntegral ( length s1 + length s2)

-}


{-

-- distance1 :: String -> String -> Float
distance1 :: String -> String -> Int
distance1 [] [] = 0
distance1 string [] = ( length string + 0) `div` ( length string  + 0)


distance1 :: String -> String -> Float
distance1 [] [] = 0
distance1 string [] = ( length string + 0) / length string
-}


