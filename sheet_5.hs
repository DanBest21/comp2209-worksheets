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
foldExpr f f' v (Add l r) = f v (foldExpr f f' (foldExpr f f' v l) r)
foldExpr f f' v (Sub l r) = f' v (foldExpr f' f (foldExpr f f' v l) r)

eval :: Expr -> Int
eval e = foldExpr (+) (-) 0 e

size :: Expr -> Int
size e = foldExpr (\x acc -> acc + 1) (\x acc -> acc + 1) 0 e

-- Exercise 4
data Prop = Const Bool | Var Char | Not Prop | And Prop Prop | Imply Prop Prop
data PropForm = Negative | Positive | Mixed deriving (Eq, Show)

getForm :: Prop -> PropForm
getForm (Const n) = Positive
getForm (Var n) = Positive
getForm (Not n) = Negative
getForm (And n m) | p1 == p2    = p1
                  | otherwise   = Mixed
                where p1 = getForm n
                      p2 = getForm m
getForm (Imply n m) | p1 /= p2  = p2
                    | otherwise = Mixed
                where p1 = getForm n
                      p2 = getForm m

-- Exercise 5
data Pair a b = P (a, b) deriving (Show)

instance Functor (Pair a) where
      fmap f (P (a, b)) = P (a, f b)

data Fun a b = F (a -> b)

instance Functor (Fun a) where
      fmap f (F g) = F (f . g)

fromFun :: Fun a b -> (a -> b)
fromFun (F g) = g

-- It would not be possible to make Fun into a functor if it was defined as F (b -> a) since fmap 
-- is expecting a function from a -> b (?)

-- Exercise 6
data LTree a = Leaf' a | Node' (LTree a) (LTree a) 
data Direction a = L (LTree a) | R (LTree a)
type Trail a = [Direction a]
type Zipper a = (LTree a, Trail a)

goLeft, goRight, goUp :: (LTree a, Trail a) -> (LTree a, Trail a)
goLeft (Node' l r, ts) = (l, L r:ts)
goRight (Node' l r, ts) = (r, R l:ts)
goUp (t, L r : ts) = (Node' t r, ts)
goUp (t, R l : ts) = (Node' l t, ts)

inc2LR :: LTree Int -> LTree Int
-- inc2LR (Node' )

-- Exercise 7

-- Exercise 8