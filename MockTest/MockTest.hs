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
isNBranching n (Leaf a) = True
isNBranching n (Branch []) = n == 0
isNBranching n (Branch (x:xs)) = length (x:xs) == n && all (isNBranching n) (x:xs)

prune :: Int -> Rose a -> Rose a
prune n (Leaf a) = Leaf a
prune n (Branch []) = Branch []
prune n (Branch children) = Branch ( map (prune n) (take n children) )


---------------------------------------------------------------------------------
-- QUESTION 2
---------------------------------------------------------------------------------

applyNTimes :: Monad m => m a -> (a -> m a) -> Int -> m [a]
applyNTimes mx mf 0 = return []
applyNTimes mx mf n = do
    x0 <- mx
    go n x0
    where
        go 0 x = return [x]
        go k x = do
            x' <- mf x
            xs <- go (k-1) x'
            return (x:xs)


---------------------------------------------------------------------------------
-- QUESTION 3
---------------------------------------------------------------------------------

gameOver :: NimGame Bool
gameOver = undefined 

takeTokens :: Int -> Heap -> NimGame ()
takeTokens n h = undefined

---------------------------------------------------------------------------------
-- QUESTION 4
---------------------------------------------------------------------------------

isMagicSquare :: [[Int]] -> Bool
isMagicSquare = undefined

---------------------------------------------------------------------------------
-- QUESTION 5
---------------------------------------------------------------------------------

circuit :: Expr -> Circuit
circuit exp = undefined
