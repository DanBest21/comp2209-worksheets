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