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

goRoot :: Zipper a -> Zipper a
goRoot (t, []) = (t, [])
goRoot z = goRoot $ goUp z

getParentValue :: Trail a -> Maybe a
getParentValue [] = Nothing
getParentValue (L x c r : ts) = Just (x)
getParentValue (R x c r : ts) = Just (x)

insertInCorrectPosition :: Ord a => a -> Zipper a -> Zipper a
insertInCorrectPosition n (Leaf, ts) = ((Node (Leaf) n 1 (Leaf)), ts)
insertInCorrectPosition n (Node l x c r, []) | n == x    = (Node l x c r, [])
                                             | n < x     = insertInCorrectPosition n $ goLeft(t, [])
                                             | otherwise = insertInCorrectPosition n $ goRight(t, [])
      where t = Node l x c r
insertInCorrectPosition n (Node l x c r, ts) | n == x    = (Node l x c r, ts)
                                             | bLeft     = insertInCorrectPosition n $ goLeft(t, ts)
                                             | bRight    = insertInCorrectPosition n $ goRight(t, ts)
                                             | otherwise = insertInCorrectPosition n $ goUp(t, ts)
      where parent = fromJust $ getParentValue ts
            bParent = n < parent
            bValue = n < x
            bLeft = (bValue && bParent) || (bValue && not(bParent))
            bRight = (not(bValue) && not(bParent)) || (not(bValue) && bParent)
            t = Node l x c r

insertFromCurrentNode :: Ord a => a -> Zipper a -> Zipper a
insertFromCurrentNode n (Leaf, ts) = insertInCorrectPosition n (Leaf, ts)
insertFromCurrentNode n (Node l x c r, ts) | n == x    = (Node l x c r, ts)
                                           | otherwise = insertInCorrectPosition n $ goRoot (Node l x c r, ts)