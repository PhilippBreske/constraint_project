{-# LANGUAGE DataKinds #-}

module Main where

import Language.Hasmtlib
import Prelude hiding ((&&), (||), not, and, or, all, any)
import Control.Monad (replicateM, forM_)
import Data.List

boardtest :: [[Expr 'IntSort]]
boardtest = [[33,2,1],[0,1,5],[1,2,3]]

main :: IO ()
main = do
  let n = 5
  let top_c = [2,3,2,1,4]
  let bottom_c = [1,2,3,5,2]
  let left_c = [2,3,2,3,1]
  let right_c = [2,3,2,1,3]
  res <- solveWith @SMT (solver $ debugging noisy z3) $ do
    setLogic "QF_LIA"
    board <- replicateM n $ replicateM n $ var @IntSort
    
    -- assert every entry is greater 0
    forM_ (concat board) $ assert . (>? 0)

    -- assert every entry is less then n + 1
    forM_ (concat board) $ assert . (<=? (encode (fromIntegral n :: Integer)))
    
    -- assert every entry in a row is unique
    forM_ board $ \row -> do
      forM_ (indexedArray row) $ \(i, vi) -> 
        forM_ (drop (i+1) (indexedArray row)) $ \(_, vj) -> 
          assert $ vi /== vj  

    -- assert every entry in a column is unique
    forM_ (transpose board) $ \row -> do
      forM_ (indexedArray row) $ \(i, vi) -> 
        forM_ (drop (i+1) (indexedArray row)) $ \(_, vj) -> 
          assert $ vi /== vj  

    -- assert the top constraints
    forM_ (indexedArray top_c) $ \(i, vi) ->
      assert $ (visibleCount ((transpose board) !! i)) === vi 

    -- assert the bottom constraint
    forM_ (indexedArray bottom_c) $ \(i, vi) ->
      assert $ (visibleCount ((getBottomToTop board) !! i)) === vi

    -- assert the left constraints
    forM_ (indexedArray left_c) $ \(i, vi) ->
      assert $ (visibleCount (board !! i)) === vi

    -- assert the right constraints 
    forM_ (indexedArray right_c) $ \(i, vi) ->
      assert $ (visibleCount ((getLeftToRight board) !! i)) === vi

    --assert $ (visibleCount (board !! 1)) === 2
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
