import Control.Monad.State
import Data.List

------------------------------------
---------QUESTION ONE---------------
------------------------------------

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

height :: Tree a -> Int
height (Empty) = 0
height (Node _ l r) = 1 + max (height l) (height r)

mirror :: Tree a -> Tree a
mirror (Empty) = Empty
mirror (Node x l r) = Node x (mirror r) (mirror l)

------------------------------------
---------QUESTION TWO---------------
------------------------------------
safeChain :: (a -> Maybe a) -> Int -> a -> Maybe a
safeChain _ 0 acc = Just acc
safeChain f n acc = case f acc of
    Just s -> safeChain f (n-1) s
    Nothing -> Nothing

------------------------------------
---------QUESTION THREE-------------
------------------------------------
decrement :: State Int ()
decrement = do
    x <- get
    put (x-1)

decrementN :: Int -> State Int ()
decrementN 0 = pure ()
decrementN n = do 
    decrement
    decrementN (n-1)

------------------------------------
---------QUESTION FOUR--------------
------------------------------------
chunks :: Int -> [a] -> [[a]]
chunks 0 _ = []
chunks _ [] = []
chunks n xs = [(take n xs)] ++ chunks n (drop n xs)
--Or could be => -- chunks n xs = take n xs : chunks n (drop n xs)

------------------------------------
---------QUESTION FIVE--------------
------------------------------------
data Expr
  = T
  | F
  | And Expr Expr
  | Or Expr Expr
  deriving (Eq, Show)

simplify :: Expr -> Expr
simplify (And F _) = F
simplify (And _ F) = F
simplify (And T T) = T
simplify (And a b) = simplify (And (simplify a) (simplify b))

simplify (Or T _) = T
simplify (Or _ T) = T
simplify (Or F F) = F
simplify (Or a b) = simplify (Or (simplify a) (simplify b))

simplify a = a

--Quicker method:

-- simplify :: Expr -> Expr

-- simplify T = T
-- simplify F = F

-- simplify (And a b) =
--   case (simplify a, simplify b) of
--     (F, _) -> F
--     (_, F) -> F
--     (T, x) -> x
--     (x, T) -> x

-- simplify (Or a b) =
--   case (simplify a, simplify b) of
--     (T, _) -> T
--     (_, T) -> T
--     (F, x) -> x
--     (x, F) -> x