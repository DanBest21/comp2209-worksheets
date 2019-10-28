-- Exercise A1
histogram' :: Int -> [Int] -> [Int]
histogram' n [] = []
histogram' n (x:xs) = [x `div` n] ++ histogram' n xs

count :: Int -> [Int] -> Int
count n xs = length [ x | x <- xs, x == n ]

histogram :: Int -> [Int] -> [Int]
histogram n xs | n <= 0    = error "n cannot be negative or equal to zero!" 
               | otherwise = [ count x (divlist) | x <- [0..maximum(divlist)] ] where divlist = histogram' n xs

-- Exercise A2
approxSqrt' :: Double -> Double -> Double -> Double
approxSqrt' n d epsilon | abs (n - sqrt d) < epsilon = n
                        | otherwise                  = approxSqrt' ((n + d / n) / 2) d epsilon

approxSqrt :: Double -> Double -> Double
approxSqrt d epsilon | d < 0        = error "d cannot be negative!"
                     | epsilon <= 0 = error "epsilon cannot be negative or equal to zero!"
                     | otherwise    = approxSqrt' 1 d epsilon

-- Exercise A3
subsequence :: Eq a => [a] -> Int -> Int -> [a]
subsequence xs i n = [ x | (x, index) <- zip xs [1..], index <= n, index >= i ] 

subsequences :: Eq a => [a] -> Int -> Int -> [[a]]
subsequences xs i n | i == n && (n-1) == 0 = []
                    | i == n               = subsequences xs 0 (n-1)
                    | otherwise            = subsequence xs (i+1) n : subsequences xs (i+1) n

-- longestCommonSubsequence :: Eq a => [[a]] -> [a]
-- longestCommonSubsequence xss = [ x | xs' <- xss, xs <- subsequences xs' (length xs'), x <- xs ]