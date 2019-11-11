import Data.List
import Data.Function
import Data.Tuple

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
type Metric a = Point a -> Point a -> Double
type Point a = (a, a)

neighbours :: Int -> Metric a -> Point a -> [Point a] -> [Point a]
neighbours k d p xs | k >= 0     = map fst $ take k $ sortBy (compare `on` snd) [ (x, d p x) | x <- xs ]
                    | otherwise = error  "k cannot be negative!"

-- Exercise A5
getNumberOfPairs :: Eq a => a -> [(a, a)] -> Int
getNumberOfPairs x xs = length [ (x', y) | (x', y) <- xs, (x' == x) ]

minBondings :: Eq a => [a] -> [(a, a)] -> (a, Int)
minBondings xs ys = head $ sortBy (compare `on` snd) [ (x, y) | x <- xs, let y = getNumberOfPairs x ys ]

getPossibleBondings :: Eq a => (a -> a -> Bool) -> a -> [a] -> [(a, a)]
getPossibleBondings p x xs = [ (x, y) | y <- filter (p x) xs, x /= y ]

getAllPossibleBondings :: Eq a => (a -> a -> Bool) -> [a] -> [(a, a)]
getAllPossibleBondings p xs = nub [ (x, y) | x' <- xs, (x, y) <- getPossibleBondings p x' xs ]

fromMaybePair :: Eq a => Maybe (a, a) -> (a, a)
fromMaybePair (Just a) = a
fromMaybePair Nothing = error "Cannot find bonding!"

takeBonding :: Eq a => a -> [(a, a)] -> [(a, a)]
takeBonding x xs = [firstPair, secondPair]
                where firstPair  = fromMaybePair (find (\(y, _) -> y == x) xs)
                      secondPair = swap firstPair

removeBondings :: Eq a => (a, a) -> [(a, a)] -> [(a, a)]
removeBondings (x, y) xs = [ (x', y') | (x', y') <- xs, x' /= x && x' /= y, y' /= x && y' /= y ]

findBondings :: Eq a => (a -> a -> Bool) -> [a] -> [(a, a)] -> [(a, a)]
findBondings p [] ys = []
findBondings p xs ys | snd minBonding /= 0 = selectedBondings ++ findBondings p (filter (`notElem` [x, y]) xs) (removeBondings selectedBonding ys)
                     | otherwise           = []
                where minBonding = minBondings xs ys
                      selectedBondings = takeBonding (fst minBonding) ys
                      selectedBonding = head selectedBondings
                      x = fst selectedBonding
                      y = snd selectedBonding

findBonding :: Eq a => (a -> a -> Bool) -> [a] -> Maybe [(a, a)]
findBonding p xs | length xs == length bondings = Just (bondings)
                 | otherwise                    = Nothing
                where bondings = findBondings p xs (getAllPossibleBondings p xs)