{-# LANGUAGE DeriveGeneric #-}
--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2019

--EXERCISE A9 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES


module Exercises (optimalPower,Instruction(..),Stack,SMProg) where

import GHC.Generics (Generic,Generic1)
import Control.DeepSeq


data Instruction = Add | Mul | Dup | Pop deriving (Eq,Ord,Show,Generic)
type Stack = [Int]
type SMProg = [Instruction] 

instance NFData (Instruction)

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