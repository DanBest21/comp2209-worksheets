{-# LANGUAGE DeriveGeneric #-}
--SKELETON FILE FOR COURSEWORK 1 for COMP2209, 2019
--CONTAINS ALL FUNCTIONS REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES
--Julian Rathke, Oct 2019

module Exercises (histogram,approxSqrt,longestCommonSubsequence,neighbours,findBonding,insertFromCurrentNode,VTree(..),Direction(..),Trail(..),Zipper(..),Instruction(..),Stack,SMProg,evalInst,findMaxReducers,optimalPower) where

-- The following two imports are needed for testing, do not delete
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq

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
type Point a = (a,a)
type Metric a = (Point a) -> (Point a) -> Double

neighbours ::  Int -> Metric a -> Point a -> [Point a] -> [Point a]
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
data VTree a = Leaf | Node (VTree a) a Int (VTree a) deriving (Eq,Show,Generic,Generic1)
data Direction a = L a Int (VTree a) | R a Int (VTree a) deriving (Eq,Show,Generic,Generic1)
type Trail a = [Direction a]
type Zipper a = (VTree a, Trail a)

instance NFData a => NFData (VTree a)
instance NFData a => NFData (Direction a)

goLeft, goRight, goUp :: Zipper a -> Zipper a
goLeft (Node (Node l' x' c' r') x c r, ts) = (Node l' x' (c'+1) r', L x c r : ts)
goLeft (Node l x c r, ts) = (l, L x c r : ts)
goRight (Node l x c (Node l' x' c' r'), ts) = (Node l' x' (c'+1) r', R x c l : ts)
goRight (Node l x c r, ts) = (r, R x c l : ts)
goUp (t, L x c r : ts) = (Node t x (c+1) r, ts)
goUp (t, R x c l : ts) = (Node l x (c+1) t, ts)

goRoot :: Zipper a -> Zipper a
goRoot (t, []) = (t, [])
goRoot z = goRoot $ goUp z

insertInCorrectPosition :: Ord a => a -> a -> Bool -> Zipper a -> Zipper a
insertInCorrectPosition n p bUp (Leaf, ts) = ((Node (Leaf) n 1 (Leaf)), ts)
insertInCorrectPosition n p bUp (Node l x c r, []) | n == x    = (Node l x c r, [])
                                                   | n < x     = insertInCorrectPosition n x False $ goLeft(t, [])
                                                   | otherwise = insertInCorrectPosition n x False $ goRight(t, [])
      where t = Node l x c r
insertInCorrectPosition n p (False) (Node l x c r, ts) | n == x    = (Node l x c r, ts)
                                                       | bLeft     = insertInCorrectPosition n x False $ goLeft(t, ts)
                                                       | bRight    = insertInCorrectPosition n x False $ goRight(t, ts)
                                                       | otherwise = insertInCorrectPosition n x True $ goUp(t, ts)
      where bPrevious = n < p
            bCurrent = n < x
            bLeft = (bPrevious && bCurrent) || (not(bPrevious) && bCurrent)
            bRight = (not(bPrevious) && not(bCurrent)) || (bPrevious && not(bCurrent))
            t = Node l x c r
insertInCorrectPosition n p (True) (Node l x c r, ts) | n == x    = (Node l x c r, ts)
                                                      | bLeft     = insertInCorrectPosition n x False $ goRight $ goLeft(t, ts)
                                                      | bRight    = insertInCorrectPosition n x False $ goLeft $ goRight(t, ts)
                                                      | otherwise = insertInCorrectPosition n x True $ goUp(t, ts)
      where bPrevious = n < p
            bCurrent = n < x
            bLeft = (not(bPrevious) && bCurrent)
            bRight = (bPrevious && not(bCurrent))
            t = Node l x c r

insertFromCurrentNode :: Ord a => a -> Zipper a -> Zipper a
insertFromCurrentNode n (Leaf, ts) = insertInCorrectPosition n n True (Leaf, ts)
insertFromCurrentNode n (Node l x c r, ts) | n == x    = (Node l x c r, ts)
                                           | otherwise = insertInCorrectPosition n x True $ (Node l x c r, ts)

-- Exercise A7
data Instruction = Add | Mul | Dup | Pop deriving (Eq,Ord,Show,Generic)
type Stack = [Int]
type SMProg = [Instruction] 

instance NFData (Instruction)

evalInst :: Stack -> SMProg -> Stack
evalInst [] p = error "Stack cannot be empty at start of instruction process."
evalInst s [] = s
evalInst (x : []) (Add : p) = error "Cannot perform Add operation on stack with one element!"
evalInst (x : []) (Mul : p) = error "Cannot perform Mul operation on stack with one element!"
evalInst (x : y : s) (Add : p) = evalInst ((x + y) : s) p
evalInst (x : y : s) (Mul : p) = evalInst ((x * y) : s) p
evalInst (x : s) (Dup : p) = evalInst (x : x : s) p
evalInst (x : s) (Pop : p) = evalInst s p

-- Exercise A8
negativeNumbers :: Stack -> [Int]
negativeNumbers s = [ x | x <- s, x < 0 ]

leastNegativeNumber :: Stack -> Int
leastNegativeNumber s | list /= [] && length list `mod` 2 /= 0 = last $ sort $ list
                      | otherwise                              = 0
                  where list = negativeNumbers s

nextNegativeNumber :: Stack -> Int
nextNegativeNumber s = head $ negativeNumbers s

negativeNumbersLeft :: Stack -> Int
negativeNumbersLeft s = length $ negativeNumbers s

findMaxReducersHelper :: Stack -> SMProg -> Int -> [SMProg]
findMaxReducersHelper (x : []) acc n = [acc]
findMaxReducersHelper (x : y : s) acc n | x == 0 && y == 0                            = (findMaxReducersHelper (y : s) pop n) ++ (findMaxReducersHelper ((x + y) : s) add n) ++ (findMaxReducersHelper ((x * y) : s) mul n)
                                        | x == 0                                      = (findMaxReducersHelper (y : s) pop n) ++ (findMaxReducersHelper ((x + y) : s) add n)
                                        | y == n && n /= 0                            = findMaxReducersHelper ((x + y) : s) add n
                                        | x == n && n /= 0 && not(bNegative)          = findMaxReducersHelper (y : s) pop n
                                        | abs (x + y) < abs (x * y)                   = findMaxReducersHelper ((x * y) : s) mul n
                                        | otherwise                                   = findMaxReducersHelper ((x + y) : s) add n
                  where pop = acc ++ [Pop]
                        add = acc ++ [Add]
                        mul = acc ++ [Mul]
                        bNegative = x < 0 && y < 0
                        
findMaxReducers :: Stack -> [SMProg]
findMaxReducers [] = []
findMaxReducers s = findMaxReducersHelper s [] (leastNegativeNumber s)

-- Exercise A9
type BigStack = [Integer]

bigEvalInst :: BigStack -> SMProg -> BigStack
bigEvalInst [] p = error "Stack cannot be empty at start of instruction process." 
bigEvalInst s [] = s
bigEvalInst (x : []) (Add : sm) = error "Cannot perform Add operation on stack with one element!"
bigEvalInst (x : []) (Mul : sm) = error "Cannot perform Mul operation on stack with one element!"
bigEvalInst (x : y : s) (Add : sm) = bigEvalInst ((x + y) : s) sm
bigEvalInst (x : y : s) (Mul : sm) = bigEvalInst ((x * y) : s) sm
bigEvalInst (x : s) (Dup : sm) = bigEvalInst (x : x : s) sm
bigEvalInst (x : s) (Pop : sm) = bigEvalInst s sm

closest2nExponent :: Int -> Int -> Int -> Int
closest2nExponent n x i | n >= x    = closest2nExponent n (x * 2) (i + 1)
                        | otherwise = i

generateSequencesToLimit :: Int -> Int -> Int -> SMProg -> [SMProg]
generateSequencesToLimit n i j acc | j == n        = [acc]
                                   | i == j        = generateSequencesToLimit n (i + 1) j dup
                                   | i == n        = generateSequencesToLimit n i (j + 1) mul
                                   | otherwise     = generateSequencesToLimit n (i + 1) j dup ++ generateSequencesToLimit n i (j + 1) mul
                  where dup = acc ++ [Dup]
                        mul = acc ++ [Mul]

optimalPower :: Int -> SMProg
optimalPower n | n <= 0    = error "Input value cannot be 0 or below!"
               | otherwise = head [ sq | i <- [(closest2nExponent n 2 0)..], sq <- generateSequencesToLimit i 0 0 [], (head $ bigEvalInst [2] sq) == (2 ^ n) ]