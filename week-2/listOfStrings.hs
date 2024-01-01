-- List of strings
{-
Write a function headOrLast :: [String] -> Char -> [String] that, 
given a list of strings and a character, 
evaluates to a list with all the strings of the input list that either begin or end with the input character.
-}

headOrLast :: [String] -> Char -> [String]
headOrLast inputList x = [ y | y <- inputList, length y > 0, head y == x || last y == x ]
