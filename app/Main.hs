{-# LANGUAGE DataKinds #-}

module Main where

import Parser (getPuzzle, size, top, bottom, left, right, prefilled)
import Language.Hasmtlib
import Prelude hiding ((&&), (||), not, and, or, all, any)
import Control.Monad (replicateM, forM_)
import Data.List

boardtest :: [[Expr 'IntSort]]
boardtest = [[33,2,1],[0,1,5],[1,2,3]]


main :: IO ()
main = main1 "https://www.chiark.greenend.org.uk/~sgtatham/puzzles/js/towers.html#5:/3/3///3/2/2//3////2//3///2/,m1f2d"

main1 :: String -> IO ()
main1 s = do
  let puzzle = getPuzzle s
  let n = size puzzle 
  let top_c = map encodeInt (top puzzle)
  let bottom_c = map encodeInt (bottom puzzle)
  let left_c = map encodeInt (left puzzle)
  let right_c = map encodeInt (right puzzle)
  let set_towers = prefilled puzzle
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
      assert $ ((board !! x) !! y) === (encode (fromIntegral h :: Integer))

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

encodeInt :: Int -> Expr 'IntSort
encodeInt x = encode (fromIntegral x :: Integer)


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
