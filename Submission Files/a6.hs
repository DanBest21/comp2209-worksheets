{-# LANGUAGE DeriveGeneric #-}

--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2019

--EXERCISE A6 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES

module Exercises (insertFromCurrentNode,VTree(..),Direction(..),Trail(..),Zipper(..)) where

-- The following two imports are needed for testing, do not delete
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq 
import Data.Maybe

data VTree a = Leaf | Node (VTree a) a Int (VTree a) deriving (Eq,Show,Generic,Generic1)
data Direction a = L a Int (VTree a) | R a Int (VTree a) deriving (Eq,Show,Generic,Generic1)
type Trail a = [Direction a]
type Zipper a = (VTree a, Trail a)

instance NFData a => NFData (VTree a)
instance NFData a => NFData (Direction a)


-- Exercise A6
goLeft, goRight, goUp :: Zipper a -> Zipper a
goLeft (Node (Node l' x' c' r') x c r, ts) = (Node l' x' (c'+1) r', L x c r : ts)
goLeft (Node l x c r, ts) = (l, L x c r : ts)
goRight (Node l x c (Node l' x' c' r'), ts) = (Node l' x' (c'+1) r', R x c l : ts)
goRight (Node l x c r, ts) = (r, R x c l : ts)
goUp (t, L x c r : ts) = (Node t x (c+1) r, ts)
goUp (t, R x c l : ts) = (Node l x (c+1) t, ts)

getParentValue :: Trail a -> Maybe a
getParentValue [] = Nothing
getParentValue (L x c r : ts) = Just (x)
getParentValue (R x c r : ts) = Just (x)

getRootValue :: Trail a -> Maybe a
getRootValue [] = Nothing
getRootValue (L x c r : []) = Just (x)
getRootValue (R x c r : []) = Just (x)
getRootValue (L x c r : ts) = getRootValue ts
getRootValue (R x c r : ts) = getRootValue ts

mkTree :: Ord a => [a] -> Zipper a
mkTree = foldl (\z -> \x -> insertFromCurrentNode x z) (Leaf,[])

insertFromCurrentNode :: Ord a => a -> Zipper a -> Zipper a
insertFromCurrentNode n (Leaf, ts) = ((Node (Leaf) n 1 (Leaf)), ts)
insertFromCurrentNode n (Node l x c r, []) | n == x    = (Node l x c r, [])
                                           | n < x     = insertFromCurrentNode n $ goLeft(t, [])
                                           | otherwise = insertFromCurrentNode n $ goRight(t, [])
      where t = Node l x c r
insertFromCurrentNode n (Node l x c r, ts) | n == x    = (Node l x c r, ts)
                                           | bLeft     = insertFromCurrentNode n $ goLeft(t, ts)
                                           | bRight    = insertFromCurrentNode n $ goRight(t, ts)
                                           | otherwise = insertFromCurrentNode n $ goUp(t, ts)
      where parent = fromJust $ getParentValue ts
            root = fromJust $ getRootValue ts
            bRoot = n < root
            bRoot' = x < root
            bParent = n < parent
            bValue = n < x
            bLeft = (bValue && bRoot && bRoot' && bParent) || (not(bRoot) && not(bRoot') && bValue && not(bParent))
            bRight = (not(bValue) && not(bRoot) && not(bRoot') && not(bParent)) || (bRoot && bRoot' && not(bValue) && bParent)
            t = Node l x c r