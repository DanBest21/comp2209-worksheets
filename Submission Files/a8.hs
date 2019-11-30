{-# LANGUAGE DeriveGeneric #-}
--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2019

--EXERCISE A8 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES


module Exercises (findMaxReducers,Instruction(..),Stack,SMProg) where

import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import Data.List

data Instruction = Add | Mul | Dup | Pop deriving (Eq,Ord,Show,Generic)
type Stack = [Int]
type SMProg = [Instruction]

instance NFData (Instruction)

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