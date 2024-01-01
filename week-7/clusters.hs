-- Clusters

{-
Write a function clusters :: (String -> String -> Float) -> Float -> [String] -> [[String]] that is given:

f :: String -> String -> Float, a distance function, (week 5).
d :: Float, a distance limit
ss :: [String], a list of strings

For each string s in ss, the function clusters computes a “cluster”, which is a list of “close” strings in ss (strings that are at most distance d from the s). The list of strings close to s should also contain s (if the distance function allows).
The strings in clusters and the list of clusters may be in any order. The grader sorts them.

Calling this function with the function of exercise Distance functions 1

distance1, d=0.3 and ss=["aaabc", "aabdd", "a", "aa", "abdd", "bcbcb", "", "abcdefghij"] 
should return [[""],["a","aa"],["a","aa","aaabc"],["aa","aaabc","aabdd","bcbcb"],["aaabc","aabdd","abdd"],["aaabc","bcbcb"],["aabdd","abdd"],["abcdefghij"]] (in some order).

distance2, d=0.2 and ss=["123a","456789b","45","abc", "ab1", "a12", "abcdefghij"] 
should return [[],[],[],["123a","45","456789b","a12"],["123a","45","456789b","a12"],["45","456789b"],["45","456789b"]] (in some order).
-}

clusters :: (String -> String -> Float) -> Float -> [String] -> [[String]]
clusters func distance list = foldl (\acc x -> (filter (\y -> ( func x y ) <= distance) list): acc ) [] list

-- same with foldr
clusters2 :: (String -> String -> Float) -> Float -> [String] -> [[String]]
clusters2 func distance list = foldr filterByDistance [] list
  where filterByDistance elem acc = filter (\y -> ( func elem y ) <= distance) list : acc



-- distance1 function is here for testing clusters function
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