module Code (countLeaves, mapRose) where

import Control.Monad.State
import Data.List

data Rose a = Leaf a | Branch [Rose a]

------------------------------------
---------QUESTION ONE---------------
------------------------------------

countLeaves :: Rose a -> Int
countLeaves (Leaf _) = 1
countLeaves (Branch xs) = sum (map countLeaves xs)

mapRose :: (a->b) -> Rose a -> Rose b
mapRose f (Leaf l)= Leaf (f l)
mapRose f (Branch xs) = Branch (map (mapRose f) xs)

------------------------------------
---------QUESTION TWO---------------
------------------------------------

applyNTimes :: Monad m => m a -> (a -> m a) -> Int -> m [a]
applyNTimes mx mf 0 = mx >>= (\x -> pure [x])
applyNTimes mx mf n = do
    x <- mx
    xs <- applyNTimes (mf x) mf (n-1)
    pure (x:xs)
    
------------------------------------
---------QUESTION THREE-------------
------------------------------------
tick :: State Int Int
tick = do
    x <- get
    put (x+1)
    return x

------------------------------------
---------QUESTION FOUR--------------
------------------------------------
columnSums :: [[Int]] -> [Int]
columnSums xss = map sum (transpose xss)

------------------------------------
---------QUESTION FIVE--------------
------------------------------------
data Expr = Val Int | Add Expr Expr | Mul Expr Expr

eval :: Expr -> Int
eval (Val a) = a
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y