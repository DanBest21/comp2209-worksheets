import Data.List
import Data.Function
import Data.Tuple
import Data.Maybe

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
getAllPossibleBondings p xs = [ (x, y) | x' <- xs, (x, y) <- getPossibleBondings p x' xs ]

takeBonding :: Eq a => a -> [(a, a)] -> [(a, a)]
takeBonding x xs = [firstPair, secondPair]
                where firstPair  = fromJust (find (\(y, _) -> y == x) xs)
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

-- Exercise A6
data VTree a = Leaf | Node (VTree a) a Int (VTree a) deriving (Show)

data Direction a = L a Int (VTree a) | R a Int (VTree a) deriving (Show)
type Trail a = [Direction a]
type Zipper a = (VTree a, Trail a) 

goLeft, goRight, goUp :: Zipper a -> Zipper a
goLeft (Node (Node l' x' c' r') x c r, ts) = (Node l' x' (c'+1) r', L x c r : ts)
goLeft (Node l x c r, ts) = (l, L x c r : ts)
goRight (Node l x c (Node l' x' c' r'), ts) = (Node l' x' (c'+1) r', R x c l : ts)
goRight (Node l x c r, ts) = (r, R x c l : ts)
goUp (t, L x c r : ts) = (Node t x (c+1) r, ts)
goUp (t, R x c l : ts) = (Node l x (c+1) t, ts)

getParentValue :: Trail a -> Maybe a
getParentValue [] = Nothing
getParentValue (L x c r : ts) = Just (x)
getParentValue (R x c r : ts) = Just (x)

getRootValue :: Trail a -> Maybe a
getRootValue [] = Nothing
getRootValue [L x c r, _] = Just (x)
getRootValue [R x c r, _] = Just (x)
getRootValue (L x c r : ts) = getRootValue ts
getRootValue (R x c r : ts) = getRootValue ts

mkTree :: Ord a => [a] -> Zipper a
mkTree = foldl (\z -> \x -> insertFromCurrentNode x z) (Leaf,[])

insertFromCurrentNode :: Ord a => a -> Zipper a -> Zipper a
insertFromCurrentNode n (Leaf, ts) = ((Node (Leaf) n 1 (Leaf)), ts)
insertFromCurrentNode n (Node l x c r, []) | n < x     = insertFromCurrentNode n $ goLeft(t, [])
                                           | otherwise = insertFromCurrentNode n $ goRight(t, [])
      where t = Node l x c r
insertFromCurrentNode n (Node l x c r, ts) | bLeft     = insertFromCurrentNode n $ goLeft(t, ts)
                                           | bRight    = insertFromCurrentNode n $ goRight(t, ts)
                                           | otherwise = insertFromCurrentNode n $ goUp(t, ts)
      where parent = fromJust $ getParentValue ts
            root = fromJust $ getRootValue ts
            bParent = x < parent
            bParent' = n < parent
            bValue = n < x
            bLeft = (not(bParent || bParent') && bValue)
            bRight = (bParent && bParent' && not(bValue))
            t = Node l x c r

-- Exercise A7
data Instruction = Add | Mul | Dup | Pop
type Stack = [Int]
type SMProg = [Instruction]

evalInst :: Stack -> SMProg -> Stack
evalInst [] p = error "Stack cannot be empty at start of instruction process." 
evalInst s [] = s
evalInst (x : []) (Add : sm) = error "Cannot perform Add operation on stack with one element!"
evalInst (x : []) (Mul : sm) = error "Cannot perform Mul operation on stack with one element!"
evalInst (x : y : s) (Add : sm) = evalInst ((x + y) : s) sm
evalInst (x : y : s) (Mul : sm) = evalInst ((x * y) : s) sm
evalInst (x : s) (Dup : sm) = evalInst (x : x : s) sm
evalInst (x : s) (Pop : sm) = evalInst s sm

-- Exercise A8
findMaxReducers :: Stack -> [SMProg]
findMaxReducers (x : []) = []
findMaxReducers (x : y : s) | x < 1              = ([Pop] ++ findMaxReducers (y : s)) : ([Add] ++ findMaxReducers ((x + y) : s))
                            | (x + y) == (x * y) = ([Add] ++ findMaxReducers ((x + y) : s)) : ([Mul] ++ findMaxReducers ((x * y) : s))
                            | (x + y) < (x * y)  = [Mul] ++ findMaxReducers ((x * y) : s)
                            | otherwise          = [Add] ++ findMaxReducers ((x + y) : s)