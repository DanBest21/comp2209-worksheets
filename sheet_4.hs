-- Exercise 1
all' :: Eq a => (a -> Bool) -> [a] -> Bool
all' f xs = (filter f xs) == xs

any' :: Eq a => (a -> Bool) -> [a] -> Bool
any' f xs = (filter f xs) /= []

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f = foldr (\x xs -> if f x then x:xs else []) []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f xs = foldr (\x xs' b -> if f x && b then xs' True else x:xs' False) (const []) xs True

-- Exercise 2
concatInt :: Int -> Int -> Int
concatInt x y | x < 0     = (10 * x) - y
              | otherwise = (10 * x) + y

dec2Int :: [Int] -> Int
dec2Int xs = foldl concatInt 0 xs

-- Exercise 3
curry' :: ((a, b) -> c) -> (a -> (b -> c)) 
curry' f = \x y -> f (x, y)

uncurry' :: (a -> (b -> c)) -> ((a, b) -> c)
uncurry' f = \(x, y) -> f x y

-- Exercise 4
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

int2bin :: Int -> [Int]
int2bin x = reverse (unfold (<= 0) (`mod` 2) (`div` 2) x)

chop :: String -> Int -> [String]
chop s x = unfold (== []) (take x) (drop x) s

-- TO DO: Fix this
-- map' :: (a -> b) -> [a] -> [b]
-- map' f xs = unfold (== []) (\x -> f x) (drop 1) xs  

iterate' :: Eq a => (a -> a) -> a -> [a]
iterate' f x = unfold (\x -> x /= x) (\x -> x) (f) x 

-- Exercise 5
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g [] = []
altMap f g [x] = [f x]
altMap f g (x:y:xs) = f x : g y : altMap f g xs

-- Exercise 6
luhnDouble :: Int -> Int
luhnDouble n | d >= 9 = d - 9
             | otherwise = d 
            where d = (n * 2)

luhn :: [Int] -> Bool
luhn xs = (sum . altMap (*1) luhnDouble $ (reverse xs)) `mod` 10 == 0

-- Exercise 7
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

toTree :: Ord a => [a] -> Tree a
toTree [] = Leaf
toTree xs = Node (toTree (take half xs)) (xs !! half) (toTree (drop (half + 1) xs))
    where half = length xs `div` 2

-- Exercise 8
data Nat = Zero | Succ Nat deriving (Eq, Ord, Show, Read)

even' :: Nat -> Bool
even' Zero = True
even' (Succ Zero) = False
even' (Succ (Succ n)) = even' n

odd' :: Nat -> Bool
odd' n = not (even' n)

add' :: Nat -> Nat -> Nat
add' Zero m = m
add' (Succ n) m = Succ (add' n m)

mult :: Nat -> Nat -> Nat
mult (Succ Zero) m = m
mult (Succ n) m = add' m (mult n m)

-- Exercise 9
data RInt = Zero' | Succ' RInt | Pred RInt deriving Show

countOperations :: RInt -> Int -> Int -> RInt
countOperations Zero' s p = normalise' (s-p)
countOperations (Succ' n) s p = countOperations n (s+1) p
countOperations (Pred n) s p = countOperations n s (p+1)  

normalise' :: Int -> RInt
normalise' n | n == 0    = Zero'
             | n > 0     = Succ' (normalise' (n-1))
             | otherwise = Pred (normalise' (n+1))

normalise :: RInt -> RInt
normalise n = countOperations n 0 0

even'' :: RInt -> Bool
even'' Zero' = True
even'' (Succ' Zero') = False
even'' (Succ' (Succ' n)) = even'' n
even'' (Pred Zero') = False
even'' (Pred (Pred n)) = even'' n

even_ :: RInt -> Bool
even_ n = even'' (normalise n)

odd_ :: RInt -> Bool
odd_ n = not (even_ n)

add'' :: RInt -> RInt -> RInt
add'' Zero' m = m
add'' (Succ' n) m = Succ' (add_ n m)
add'' (Pred n) m = Pred (add_ n m)

add_ :: RInt -> RInt -> RInt
add_ n m = add'' (normalise n) (normalise m)

isNegative :: RInt -> Bool
isNegative (Pred n) = True
isNegative n = False

makeNegative :: RInt -> RInt
makeNegative Zero' = Zero'
makeNegative (Succ' n) = Pred (makeNegative n)
makeNegative (Pred n) = Pred (makeNegative n)

mult' :: RInt -> RInt -> RInt
mult' (Succ' Zero') m = m
mult' (Pred Zero') m = m
mult' (Succ' n) m = add'' m (mult' n m)
mult' (Pred n) m = add'' m (mult' n m)

mult_ :: RInt -> RInt -> RInt
mult_ n m | (isNegative v1) /= (isNegative v2) = makeNegative (mult' v1 v2)
          | otherwise                          = mult' v1 v2 
          where v1 = normalise n
                v2 = normalise m