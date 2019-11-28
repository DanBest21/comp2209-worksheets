{-# LANGUAGE DeriveGeneric #-}
--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2019

--EXERCISE A7 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES


module Exercises (evalInst,Instruction(..),Stack,SMProg) where

import GHC.Generics (Generic,Generic1)
import Control.DeepSeq


data Instruction = Add | Mul | Dup | Pop deriving (Eq,Ord,Show,Generic)
type Stack = [Int]
type SMProg = [Instruction] 

instance NFData (Instruction)

-- Exercise A7
evalInst s [] = s
evalInst :: Stack -> SMProg -> Stack
evalInst (x : []) (Add : p) = error "Cannot perform Add operation on stack with one element!"
evalInst (x : []) (Mul : p) = error "Cannot perform Mul operation on stack with one element!"
evalInst (x : y : s) (Add : p) = evalInst ((x + y) : s) p
evalInst (x : y : s) (Mul : p) = evalInst ((x * y) : s) p
evalInst (x : s) (Dup : p) = evalInst (x : x : s) p
evalInst (x : s) (Pop : p) = evalInst s p