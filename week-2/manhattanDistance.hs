-- Manhattan distance
{-
Manhattan distance between two points is the sum of x-distance and y-distance. Ie. Manhattan distance between (3,3) and (2,6) is 1 + 3 = 4 (distance between 3 and 2 + distance between 3 and 6)
Write a function points :: Int -> [(Int, Int)] that given an Int x, evaluates to a list of such points in 2-dimensional space (pairs of type (Int, Int)), that their Manhattan distance from origin (0, 0) is at most x.
-}


points :: Int -> [(Int, Int)]
points a = [ (x,y) | x <- [ -a..a], y <- [ -a..a], ( abs (0 - x) + abs (0 - y)) <= a ]

-- abs calculates absolute value, so no negative values