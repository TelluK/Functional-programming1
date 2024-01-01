-- Notes about Haskell

map (\x -> (x * x)) [1, 2, 3, 4, 5]

filter (\x -> x `mod` 2 == 0) [1, 2, 3, 4, 5, 6, 7, 8, 9]

map ($ 3) [(4+), (10*), (^2), sqrt]

-- get each number's absolute value and then negate it
-- using lambda:
map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24] 
-- using function composition:
map (negate . abs) [5,-3,-6,7,-3,2,-19,24] 

map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]] 
-- same using function composition:
map (negate . sum . tail) [[1..5],[3..6],[1..7]]  
-- Function composition is right-associative !!

-- filters 
filter (\x -> elem x "Hello") ['m', 'o', 'h', 'H', 'l']  -- "oHl"
filter (\x -> elem x "Hello!") "!moikka"                 -- "!o"

-- flips elem functions parameters:
filter ((flip elem) "Hello!") "!ae"     -- "!e"

-- uncurry example
filter (uncurry elem) $ zip ['!', '?', 'e'] ["Hello!", "Bonjour", "Hei!"]


map sqrt $ map abs [1.0, 2.0, -4.0]
-- using function composition:
map (sqrt . abs) [1.0, 2.0, -4.0]



-- foldl, left side fold
sum' xs = foldl (\acc x -> acc + x) 0 xs 
sum' [3,5,2,1]       -- 11
sum' = foldl (+) 0 

-- foldr, right side fold
map' :: (a -> b) -> [a] -> [b] 
map' f xs = foldr (\x acc -> f x : acc) [] xs
map' (+3) [1,2,3]    -- [4,5,6]

-- implemented a bunch of standard library functions by using folds:
maximum' :: (Ord a) => [a] -> a  
maximum' = foldr1 (\x acc -> if x > acc then x else acc)  
  
reverse' :: [a] -> [a]  
reverse' = foldl (\acc x -> x : acc) []  
  
product' :: (Num a) => [a] -> a  
product' = foldr1 (*)  
  
filter' :: (a -> Bool) -> [a] -> [a]  
filter' p = foldr (\x acc -> if p x then x : acc else acc) []  
  
head' :: [a] -> a  
head' = foldr1 (\x _ -> x)  
  
last' :: [a] -> a  
last' = foldl1 (\_ x -> x)
