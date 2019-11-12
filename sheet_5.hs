-- Exercise 1
data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = compare x y == EQ
occurs x (Node (l) y (r)) = (compare x y) == EQ || (occurs x l) || (occurs x r)

-- Exercise 2
foldTree :: (a -> b -> b) -> b -> Tree a -> b
foldTree f v (Leaf x) = f x v
foldTree f v (Node (l) x (r)) = f x (foldTree f (foldTree f v l) r)

flatten :: Tree a -> [a]
flatten t = foldTree (:) [] t

-- Exercise 3
data Expr = Val Int | Add Expr Expr | Sub Expr Expr

foldExpr :: (Int -> Int -> Int) -> (Int -> Int -> Int) -> Int -> Expr -> Int
foldExpr f f' v (Val x) = f x v
foldExpr f f' v (Add l r) = f (foldExpr f f' v l) (foldExpr f f' v r)
foldExpr f f' v (Sub l r) = f' (foldExpr f f' v l) (foldExpr f' f v r)

eval :: Expr -> Int
eval e = foldExpr (+) (-) 0 e

size :: Expr -> Int
size e = foldExpr (\x acc -> acc + 1) (\x acc -> acc + 1) 0 e

-- Exercise 4
data Prop = Const Bool | Var Char | Not Prop | And Prop Prop | Imply Prop Prop



-- Exercise 5

-- Exercise 6

-- Exercise 7

-- Exercise 8