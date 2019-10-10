-- Exercise 1:
double x = x + x
quadruple x = double (double x)

-- Exercise 2:
-- Sum is already defined in the Haskell prelude

-- Exercise 3:
c_product [] = 1
c_product (x:xs) = x * c_product xs

-- Exercise 4:
quicksort [] = []
quicksort (x:xs) = quicksort ls ++ [x] ++ quicksort rs
                    where
                        ls = [ a | a <- xs, a <= x ]
                        rs = [ a | a <- xs, a > x ]

quicktros [] = []
quicktros (x:xs) = quicktros ls ++ [x] ++ quicktros rs
                    where
                        ls = [ a | a <- xs, a > x ]
                        rs = [ a | a <- xs, a <= x ]

-- Exercise 5:
b_quicksort [] = []
b_quicksort (x:xs) = b_quicksort ls ++ [x] ++ b_quicksort rs
                    where
                        ls = [ a | a <- xs, a < x ]
                        rs = [ a | a <- xs, a > x ]

-- Exercise 6:
-- 2^3*4 = (2^3)*4 = 8*4 = 32 
-- 2*3+4*5 = (2*3)+(4*5) = 6+20 = 26
-- 2+3*4^5 = 2+(3*(4^5)) = 2+(3*1024) = 2+3072 = 3074
-- 2^2+2^2 = (2^2)+(2^2) = 4+4 = 8

-- Exercise 7:
n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

-- Exercise 8:
-- ['a', 'b', 'c'] => [Char]
-- ('a', 'b', 'c') => (Char, Char, Char)
-- ['a', 3, True] => Error
-- ('a', 3, True) => (Char, Num, Bool) / Num b -> (Char, b, Bool)
-- [(False, '0'), (True, '1')] => [(Bool, Char)]
-- ([True,False], ['0','1']) => ([Bool], [Char])
-- [tail, init, reverse] => [[a] -> [a]]
-- [] => [a]
-- 2 : 3 : [] : 4 : 5 : [] => [Num] / Num [a] -> [[a]]
-- [] : [] => [[a]]

-- Exercise 9:
bools :: [Bool]
nums :: [[Int]]
add :: Int -> Int -> Int -> Int
copy :: a -> (a, a)
apply :: (a -> b) -> a -> b
explode :: String -> [Char]

bools = undefined
nums = undefined
add a b c = a + b + c
copy a = (a, a)
apply (abs) a = abs a
explode s = s ++ " ...hic!"

-- bools => [True, False]
-- nums => [[1, 2, 3], [4, 5, 6]]
-- add 4 5 6 = 15
-- copy 5 -> (5, 5)
-- apply (abs) (-10) = 10
-- explode "hello there" = "hello there ...hic!"

-- Exercise 10:
second xs = head (tail xs)
swap (x,y) = (y,x)
pair x y = (x,y)
d_double x = x * 2
palindrome xs = reverse xs == xs
twice f x = f ( f x )

-- second :: [a] -> a
-- swap :: (a, b) -> (b, a)
-- pair :: a -> b -> (a, b)
-- d_double :: Num a => a -> a
-- palindrome :: Eq a => [a] -> Bool
-- twice :: (t -> t) -> t -> t

-- Exercise 11:
-- It is hard to compare two functions to one another to ensure that they behave exactly the same in
-- all cases. Since functions are defined by means of being from one type to another 
-- (e.g. String -> Bool), but can have completely differing implementations, it is difficult to ensure
-- that they are one in the same.