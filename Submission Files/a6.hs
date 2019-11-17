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

insertInCorrectPosition :: Ord a => a -> a -> Bool -> Zipper a -> Zipper a
insertInCorrectPosition n p bUp (Leaf, ts) = ((Node (Leaf) n 1 (Leaf)), ts)
insertInCorrectPosition n p bUp (Node l x c r, []) | n == x    = (Node l x c r, [])
                                                   | n < x     = insertInCorrectPosition n x False $ goLeft(t, [])
                                                   | otherwise = insertInCorrectPosition n x False $ goRight(t, [])
      where t = Node l x c r
insertInCorrectPosition n p (False) (Node l x c r, ts) | n == x    = (Node l x c r, ts)
                                                       | bLeft     = insertInCorrectPosition n x False $ goLeft(t, ts)
                                                       | bRight    = insertInCorrectPosition n x False $ goRight(t, ts)
                                                       | otherwise = insertInCorrectPosition n x True $ goUp(t, ts)
      where bPrevious = n < p
            bCurrent = n < x
            bLeft = (bPrevious && bCurrent) || (not(bPrevious) && bCurrent)
            bRight = (not(bPrevious) && not(bCurrent)) || (bPrevious && not(bCurrent))
            t = Node l x c r
insertInCorrectPosition n p (True) (Node l x c r, ts) | n == x    = (Node l x c r, ts)
                                                      | bLeft     = insertInCorrectPosition n x False $ goRight $ goLeft(t, ts)
                                                      | bRight    = insertInCorrectPosition n x False $ goLeft $ goRight(t, ts)
                                                      | otherwise = insertInCorrectPosition n x True $ goUp(t, ts)
      where bPrevious = n < p
            bCurrent = n < x
            bLeft = (not(bPrevious) && bCurrent)
            bRight = (bPrevious && not(bCurrent))
            t = Node l x c r

insertFromCurrentNode :: Ord a => a -> Zipper a -> Zipper a
insertFromCurrentNode n (Leaf, ts) = insertInCorrectPosition n n True (Leaf, ts)
insertFromCurrentNode n (Node l x c r, ts) | n == x    = (Node l x c r, ts)
                                           | otherwise = insertInCorrectPosition n x True $ (Node l x c r, ts)