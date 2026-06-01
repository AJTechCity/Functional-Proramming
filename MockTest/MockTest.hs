-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns -Wno-x-partial #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE NoGeneralizedNewtypeDeriving, Safe #-}

module MockTest ( isNBranching
                 , prune
                 , applyNTimes
                 , gameOver
                 , takeTokens
                 , isMagicSquare
                 , circuit
                 ) where

import Types
import Data.List

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
-- QUESTION 1
---------------------------------------------------------------------------------

isNBranching :: Int -> Rose a -> Bool
isNBranching n (Leaf l) = True
isNBranching n (Branch children) = (length children == n) && and [isNBranching n child | child <- children]

prune :: Int -> Rose a -> Rose a
prune _ (Leaf l) = Leaf l
prune n (Branch xs) = Branch [prune n x | x<-(take n xs)]

---------------------------------------------------------------------------------
-- QUESTION 2
---------------------------------------------------------------------------------

applyNTimes :: Monad m => m a -> (a -> m a) -> Int -> m [a]
applyNTimes mx _ 0 = mx >>= (\x -> pure [x])
applyNTimes mx mf n = do
    x <- mx
    xs <- applyNTimes (mf x) mf (n-1)
    pure (x:xs)

---------------------------------------------------------------------------------
-- QUESTION 3
---------------------------------------------------------------------------------

gameOver :: NimGame Bool
gameOver = do
    (x,y) <- get
    pure (x==0&&y==0)


takeTokens :: Int -> Heap -> NimGame ()
takeTokens n (First) = do
    (x,y) <- get
    put ((max 0 (x-n)), y)
takeTokens n (Second) = do
    (x,y) <- get
    put (x, (max 0 (y-n)))

---------------------------------------------------------------------------------
-- QUESTION 4
---------------------------------------------------------------------------------

checkRows :: [[Int]] -> Int -> Bool
-- checkRows xss exp = 
--     let totals = [sum xs | xs <- xss]
--         first_total = totals !! 0
--         outcome = foldr (\xs acc -> if xs==acc then acc else -1) first_total totals
--     in
--         outcome == exp

checkCols :: [[Int]] ->Int -> Bool
checkCols xss exp = checkRows (transpose xss) exp

checkLeadDiagonal :: [[Int]] -> Int -> Bool
checkLeadDiagonal xss exp = sum [(xss!!i!!i) | i<-[0..(length xss-1)]] == exp

checkTrailDiagonal :: [[Int]] -> Int -> Bool
checkTrailDiagonal xss exp = sum [xss!!(i-1)!!(length xss - i) | i<-[1..(length xss)]] == exp

isMagicSquare :: [[Int]] -> Bool
-- isMagicSquare xss = 
--     let exp = sum (xss!!0)
--     in
--         checkRows xss exp && checkCols xss exp && checkLeadDiagonal xss exp && checkTrailDiagonal xss exp

--Easier Solution
checkRows xss exp = all (== exp) [sum xs | xs <- xss]

isMagicSquare xss = 
    let rows = map sum xss
        cols = map sum (transpose xss)
        leadDiag = sum (zipWith (!!) xss [0..(length xss-1)])
        trailDiag = sum (zipWith (!!) xss (reverse [0..(length xss-1)]))
        exp = sum (xss!!0)
    in 
        all (==exp) (rows++cols++[leadDiag, trailDiag])

---------------------------------------------------------------------------------
-- QUESTION 5
---------------------------------------------------------------------------------

circuit :: Expr -> Circuit
circuit exp = undefined
