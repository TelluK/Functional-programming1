-- Gap

{-
This task is suitable for a recursive solution.

We say that character pair (c1,c2) appears in string s with gap g, 
if c1 is before c2 and there are exactly g characters between c1 and c2 in s.

Write a function gap :: (Char, Char) -> Int -> String -> Int that, 
given a pair (c1,c2), a gap g and a string s returns an Int telling how many times (c1,c2) appear in s with gap g.

For example: gap ('a','b') 1 "aaabbb" produces 2.
-}

gap :: (Char, Char) -> Int -> String -> Int
gap (_, _) _ [] = 0
gap (_, _) _ [x] = 0
gap (_, _) i string
  | i >= length string = 0
gap (x, y) i (z:string)
  | i >= length string = 0
  | x == z && string!!i == y = 1 + gap (x, y) i string
  | otherwise = gap (x, y) i string


checkChar :: Char -> (String -> Bool)
checkChar char = \str -> elem char str
-- checkChar 'a' "aaabbb"
-- elem 'a' "aaabbb"
