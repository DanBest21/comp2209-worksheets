--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2019

--EXERCISE A5 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES


module Exercises (findBonding) where

import Data.List
import Data.Function
import Data.Tuple

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