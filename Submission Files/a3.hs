--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2019

--EXERCISE A3 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES


module Exercises (longestCommonSubsequence) where

-- Exercise A3
-- Method taken from Michael McKenna's answer that can be found at https://stackoverflow.com/questions/32575630/powerset-of-a-set-with-list-comprehension-in-haskell
subsequences :: Eq a => [a] -> [[a]]
subsequences [] = [[]]
subsequences (x:xs) = [ x:ys | ys <- subsequences xs ] ++ subsequences xs

commonSubsequences :: Eq a => [[a]] -> [[a]] -> [[a]]
commonSubsequences [] yss = yss
commonSubsequences (xs:xss) [] = commonSubsequences xss (subsequences xs)  
commonSubsequences (xs:xss) yss = commonSubsequences xss [ xs' | xs' <- subsequences xs, ys <- yss, xs' == ys ]

longestList :: Eq a => [[a]] -> [a] -> [a]
longestList [] ys = ys
longestList (xs:xss) ys | length xs >= length ys = longestList xss xs 
                        | otherwise              = longestList xss ys

longestCommonSubsequence :: Eq a => [[a]] -> [a]
longestCommonSubsequence xss = longestList (commonSubsequences xss []) []