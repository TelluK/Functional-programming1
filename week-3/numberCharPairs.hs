-- Number char pairs

{-
Let us number the smaller case characters from ‘a’ to ‘z’ with numbers starting from 1, that is, ‘a’ is given 1, ‘b’ is given number 2, etc.
A function charsDivisibleBy that, given a number n, returns all the characters that have a number divisible by n.

A function charsProductOf that, given a list of numbers ns, returns all the characters that have a number that is a product of any two numbers in ns. 
You may assume that each number only appears in the list once.

Example: 
charsDivisibleBy 2 = “bdfhjlnprtvxz”
charsProductOf [2,3,4] = “fhl”
-}

listChar = ['a'.. 'z']
listNum = [1.. 26]

charNumTuples :: [(Char, Int)]
-- zip takes two lists and creates pairs in one list
charNumTuples = zip listChar listNum

charsDivisibleBy :: Int -> [Char]
charsDivisibleBy x = [ char | (char, num) <- charNumTuples, x /= 0, num `mod` x == 0 ]

-- this submission does work with smaller list, but not with bigger lists
charsProductOf :: [Int] -> [Char]
charsProductOf [] = ""
charsProductOf [_] = ""
charsProductOf xd = [ char | (char, num) <- charNumTuples, prod <- countProducts xd, prod == num]
  where countProducts :: [Int] -> [Int]
        countProducts [] = []
        countProducts (x:xd) = [x * y | y <- xd, (x * y) < 27 ] ++ countProducts xd


{-
-- this was submitted before:
charsProductOf :: [Int] -> [Char]
charsProductOf [] = ""
charsProductOf [_] = ""
charsProductOf xd = [ char | (char, num) <- charNumTuples, prod <- countProducts xd, prod == num]
  where countProducts :: [Int] -> [Int]
        countProducts [] = []
        countProducts (x:xd) = [x * y | y <- xd] ++ countProducts xd
-}
