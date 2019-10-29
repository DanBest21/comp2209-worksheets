--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2019

--EXERCISE A1 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES


module Exercises (histogram) where

-- Exercise A1
histogram' :: Int -> [Int] -> [Int]
histogram' n [] = []
histogram' n (x:xs) = [x `div` n] ++ histogram' n xs
    
count :: Int -> [Int] -> Int
count n xs = length [ x | x <- xs, x == n ]
    
histogram :: Int -> [Int] -> [Int]
histogram n xs | n <= 0    = error "n cannot be negative or equal to zero!" 
               | otherwise = [ count x (divlist) | x <- [0..maximum(divlist)] ] where divlist = histogram' n xs