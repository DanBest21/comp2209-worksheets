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

commonElements :: Eq a => [a] -> [a] -> [a]
commonElements xs ys = [ x | x <- xs, elem x ys ]
 
commonSubsequences :: Eq a => [[a]] -> [[a]] -> [[a]]
commonSubsequences [] yss = yss
commonSubsequences (xs:xss) yss = commonSubsequences xss [ xs' | xs' <- subsequences (commonElements xs (longestList yss [])), ys <- yss, xs' == ys ]

shortestList :: Eq a => [[a]] -> [a] -> [a]
shortestList [] ys = ys
shortestList (xs:xss) ys | length xs <= length ys = shortestList xss xs 
                         | otherwise              = shortestList xss ys

longestList :: Eq a => [[a]] -> [a] -> [a]
longestList [] ys = ys
longestList (xs:xss) ys | length xs >= length ys = longestList xss xs 
                        | otherwise              = longestList xss ys

longestCommonSubsequence :: Eq a => [[a]] -> [a]
longestCommonSubsequence [] = []
longestCommonSubsequence (xs:xss) = longestList (commonSubsequences ([xs] ++ xss) (subsequences (shortestList xss xs))) []

-- Exercise A4

-- Exercise A5