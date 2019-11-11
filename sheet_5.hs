-- Exercise 1
data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = compare x y == EQ
occurs x (Node (l) y (r)) = (compare x y) == EQ || (occurs x l) || (occurs x r)

-- Exercise 2
foldTree :: Tree a -> [a]
foldTree (Leaf x) = [x]
foldTree (Node (l) x (r)) = (foldTree l) ++ [x] ++ (foldTree r)

flatten :: Tree a -> [a]
flatten t = foldTree t

-- Exercise 3
data Expr = Val Int | Add Expr Expr | Sub Expr Expr

foldExpr :: (Int -> Expr -> Int) -> [Expr] -> Int
foldExpr f (Val x) = x
foldExpr f (Add x y) = f (foldExpr x)  
foldExpr 

-- Exercise 4

-- Exercise 5

-- Exercise 6

-- Exercise 7

-- Exercise 8