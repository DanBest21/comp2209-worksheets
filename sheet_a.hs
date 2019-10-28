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
-- Method taken from Michael McKenna's answer that can be found at https://stackoverflow.com/questions/32575630/powerset-of-a-set-with-list-comprehension-in-haskell
subsequences :: Eq a => [a] -> [[a]]
subsequences [] = [[]]
subsequences (x:xs) = [ x:ys | ys <- subsequences xs ] ++ subsequences xs

commonSubsequences'' :: Eq a => [a] -> [a] -> [[a]]
commonSubsequences'' xs [] = [xs]
commonSubsequences'' xs ys = [ xs' | xs' <- subsequences xs, ys' <- subsequences ys, xs' == ys' ]

commonSubsequences' :: Eq a => [[a]] -> [a] -> [[a]]
commonSubsequences' xss [] = []
commonSubsequences' xss ys = [ xs' | xs' <- xss, ys' <- subsequences ys, xs' == ys' ]

commonSubsequences :: Eq a => [[a]] -> [[a]] -> [[a]]
commonSubsequences css (xs:[]:xss) = commonSubsequences'' 
commonSubsequences css (xs:_:xss) = commonSubsequences' css xs

longestCommonSubsequence :: Eq a => [[a]] -> [a]
longestCommonSubsequence xss = 
-- longestCommonSubsequence ([]:xss) = []
-- longestCommonSubsequence (xs:[]:xss) = xs
-- longestCommonSubsequence (xs:ys:[]:xss) = head (commonSubsequences' xs ys)
-- longestCommonSubsequence (xs:ys:zs:xss) = head (commonSubsequences (commonSubsequences' xs ys) zs)