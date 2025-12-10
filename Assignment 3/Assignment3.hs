-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE NoGeneralizedNewtypeDeriving, Safe #-}

module Assignment3 (toRose, fromRose, trace, roundRobin, schedule) where

import Types
import Control.Monad.State
import Data.Functor.Identity
import Data.List 

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

{- Question 1 -}

toRose :: Free [] a -> Rose a 
toRose (Pure x) = Lf x
toRose (Free xs) = Br (map toRose xs)

fromRose :: Rose a -> Free [] a
fromRose (Lf a) = Pure a
fromRose (Br xs) = Free (map fromRose xs)

{- Question 2 -}

trace :: FreeState s a -> State ([s],s) a
trace (Pure x) = return x
trace (Free ffa) = do
    (hist, state) <- get
    let (free2, state2) = runState ffa state
    put(state2 : hist, state2)
    trace free2

{- Question 3 -}

roundRobin :: [YieldState s ()] -> State s ()
roundRobin [] = return ()
roundRobin (x:xs) = 
    case x of
        Pure () -> 
            -- Thread finished
            roundRobin xs
        Free (FLeft st) -> do
            s <- get
            let (x', s') = runState st s
            put s'
            roundRobin (x': xs)
        Free (FRight (Yield x')) -> 
            roundRobin (xs ++ [x'])

{- Question 4 -}

schedule :: [SleepState s ()] -> State s ()
schedule = undefined
