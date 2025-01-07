{-# LANGUAGE DataKinds #-}

module Main where

import Parser
import Language.Hasmtlib
import Prelude hiding ((&&), (||), not, and, or, all, any)
import Control.Monad (replicateM, forM_)
import Data.List

boardtest :: [[Expr 'IntSort]]
boardtest = [[33,2,1],[0,1,5],[1,2,3]]

main :: IO ()
main = do
  let n = 9
  let top_c = [4,0,0,3,3,2,4,0,0]
  let bottom_c = [0,4,2,4,2,0,3,4,4]
  let left_c = [4,0,2,3,2,4,0,3,0]
  let right_c = [0,2,3,0,1,4,3,4,4]
  let set_towers = [((0,4),3),((1,1),9),((1,3),2),((2,8),4),((2,5),1),((3,1),1),((3,7),8),((4,2),1),((4,5),3),((5,7),4),((6,6),9),((7,0),4),((8,6),5)]  
  res <- solveWith @SMT (solver $ debugging noisy z3) $ do
    setLogic "QF_LIA"
    board <- replicateM n $ replicateM n $ var @IntSort
    
    -- assert every entry is greater 0
    forM_ (concat board) $ assert . (>? 0)

    -- assert every entry is less then n + 1
    forM_ (concat board) $ assert . (<=? (encode (fromIntegral n :: Integer)))
   
    -- assert every already set towers
    forM_ set_towers $ \tower -> do
      let x = fst(fst tower)
      let y = snd(fst tower)
      let h = snd tower
      assert $ ((board !! x) !! y) === h

    -- assert every entry in a row is unique
    forM_ board $ \row -> do
	assert $ distinct row

    -- assert every entry in a column is uniquehs
    forM_ (transpose board) $ \col -> do
	assert $ distinct col

    -- assert the top constraints
    forM_ (clearConstraints top_c) $ \(i, vi) ->
      assert $ (visibleCount ((transpose board) !! i)) === vi 

    -- assert the bottom constraint
    forM_ (clearConstraints bottom_c) $ \(i, vi) ->
      assert $ (visibleCount ((getBottomToTop board) !! i)) === vi

    -- assert the left constraints
    forM_ (clearConstraints left_c) $ \(i, vi) ->
      assert $ (visibleCount (board !! i)) === vi

    -- assert the right constraints 
    forM_ (clearConstraints right_c) $ \(i, vi) ->
      assert $ (visibleCount ((getLeftToRight board) !! i)) === vi

    return board
  print res

maxs :: Expr 'IntSort -> Expr 'IntSort -> Expr 'IntSort
maxs x y = ite (x >? y) x y

f :: (Expr 'IntSort, Expr 'IntSort) -> Expr 'IntSort ->(Expr 'IntSort, Expr 'IntSort) 
f (a,b) y = ((maxs a y), (ite (y >? a) (b+1) b))

visibleCount :: [Expr 'IntSort] -> Expr 'IntSort
visibleCount x = snd $ foldl f (0, 0) x

indexedArray :: [a] -> [(Int, a)]
indexedArray mat = zip [0..] mat

getBottomToTop :: [[Expr 'IntSort]] -> [[Expr 'IntSort]]
getBottomToTop x = map (\a -> reverse a) (transpose x)

getLeftToRight :: [[Expr 'IntSort]] -> [[Expr 'IntSort]]
getLeftToRight x = map (\a -> reverse a) x

clearConstraints :: [Expr 'IntSort] -> [(Int, Expr 'IntSort)]
clearConstraints x = filter (\(_,y) -> y/=0) (indexedArray x)
