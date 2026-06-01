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
toRose (Pure p)= Lf p
toRose (Free xs) = Br (map toRose xs)

fromRose :: Rose a -> Free [] a
fromRose (Lf p) = Pure p
fromRose (Br xs) = Free (map fromRose xs)

{- Question 2 -}

trace :: FreeState s a -> State ([s],s) a
trace (Pure x) = pure x
trace (Free st) = do
  (hist, s) <- get
  let (next, s') = runState st s
  put(s':hist, s')
  trace next

{- Question 3 -}

roundRobin :: [YieldState s ()] -> State s ()
roundRobin [] = pure ()
roundRobin (p:ps) = 
  case p of
    Pure _ -> roundRobin ps
    Free (FLeft s) -> roundRobin ps --TEMP CODE
    Free (FRight (Yield next)) -> roundRobin (ps++[next]) --Wants to Yield
-- roundRobin xs = 

{- Question 4 -}

schedule :: [SleepState s ()] -> State s ()
schedule = undefined
