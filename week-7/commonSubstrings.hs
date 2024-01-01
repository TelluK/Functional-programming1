-- Common (unusual) substring

{-
Write a function commonSubstring :: String -> String -> String that, given two strings s1 and s2, 
computes a common “substring” of s1 and s2 as follows. 
The function finds the earliest common character c (a character closest to the head of either s1 or s2, 
and appearing in both sequences).
The function removes c and all the characters before it in both strings, puts c in the output string,
and continues.

If there are two candidates for the earliest common character, pick the one from s1.

For example:
commonSubstring "XabcdefgY" "abcdefgXY" produces "XY"
commonSubstring "abcdefgXY" "XabcdefgY" produces "abcdefgY"

Please note that the result is not what is normally meant by substring.
-}

-- commonSubstring s1 s2 = reverse (foldl (\acc x -> x : acc) "" s1)

-- commonSubstring :: String -> String -> String
-- commonSubstring s1 s2 = reverse substring
--   where (n, substring) = foldl findSubstring (0, []) s1
--         findSubstring (n, substring) elem = (n+1, elem: substring)

-- commonSubstring :: String -> String -> String
-- commonSubstring s1 s2 = substring
--   where (n, substring) = foldl findSubstring (0, []) s1
--         findSubstring (n, substring) x = map (\y -> if x == y then (n+1, elem: substring) else (n+1, substring) ) s2 


-- these submissions don't work as intented!

commonSubstring :: String -> String -> String
commonSubstring [] [] = ""
commonSubstring _ [] = ""
commonSubstring [] _ = ""
commonSubstring s1 s2 = foldl findSubstring [] s1
  where findSubstring substring x 
          | x `elem` s2 = x: substring
          | otherwise = substring


clusters :: (String -> String -> Float) -> Float -> [String] -> [[String]]
clusters func distance list = foldl (\acc x -> (filter (\y -> ( func x y ) <= distance) list): acc ) [] list

-- same with foldr
clusters2 :: (String -> String -> Float) -> Float -> [String] -> [[String]]
clusters2 func distance list = foldr filterByDistance [] list
  where filterByDistance elem acc = filter (\y -> ( func elem y ) <= distance) list : acc

