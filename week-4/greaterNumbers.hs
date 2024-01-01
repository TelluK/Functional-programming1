-- Greater numbers
{-
Write a function nextIsGreater that, given a list of numbers, 
produces a list with all elements of the input list such that the element is followed 
by a greater number in the input list (the next number is greater).

The numbers need to be in the same order relative to each other 
in the output list as they are in the input list.

Example:
nextIsGreater [0,5,2,3,2,2,3,1] produces [0,2,2]
-}


nextIsGreater :: [Int] -> [Int]
nextIsGreater [] = []
nextIsGreater [x] = []
nextIsGreater (x:y:xs)
  | x < y = x : nextIsGreater (y:xs)
  | otherwise = nextIsGreater (y:xs)


