-- Exercise 1:
sum' = sum [ (x-1)^2 + x^3 | x <- [2,4..100] ]

-- Exercise 2:
grid :: Int -> Int -> [(Int, Int)]
grid n m = [ (x, y) | x <- [0..n], y <- [0..m] ]

square :: Int -> [(Int, Int)]
square n = [ (x, y) | x <- [0..n], y <- [0..n], x /= y ]

-- Exercise 3:
replicate' :: Int -> a -> [a]
replicate' n x = [ x | x' <- [1..n] ]

-- Exercise 4:
pyths :: Int -> [(Int, Int, Int)]
pyths n = [ (x, y, z ) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2 ]

-- Exercise 5:
factors :: Int -> [Int]
factors n = [ x | x <- [1..n-1], n `mod` x == 0 ] 

perfect :: Int -> [Int]
perfect n = [ x | x <- [1..n], sum (factors x) == x ]

-- Exercise 6:
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [ v | (k', v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..])

-- Exercise 7:
scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [ x * y | (x, x') <- zip xs [0..], (y, y') <- zip ys [0..], x' == y' ]

-- Exercise 8:
euclid :: Int -> Int -> Int
euclid x y | x == y    = x 
           | x > y     = euclid (x - y) y
           | otherwise = euclid x (y - x)

-- Exercise 9:
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x < y     = [x] ++ merge xs ([y] ++ ys)
                    | otherwise = [y] ++ merge ([x] ++ xs) ys

halve :: Ord a => [a] -> ([a], [a])
halve xs  = (take (length xs `div` 2) xs, drop (length xs `div` 2) xs)

mergeSort :: Ord a => [a] -> [a]
mergeSort xs | length xs == 1 = xs
             | otherwise      = merge (mergeSort (fst (halve xs))) (mergeSort (snd (halve xs)))