import Control.Monad.State
import Data.List

data Rose a = Leaf a | Branch [Rose a]

flatten :: Rose a -> [a]
flatten (Leaf l) = [l]
flatten (Branch []) = []
flatten (Branch xs) = foldr (\x acc -> flatten x ++ acc) [] xs

data Free f a
  = Pure a
  | Free (f (Free f a))

countPure :: Free [] a -> Int
countPure (Pure _) = 1
countPure (Free []) = 0
countPure (Free xs) = sum (map countPure xs)

moveNorth :: State (Int, Int) ()
moveNorth = do
    (x, y) <- get
    put (x, y+1)

moveSouth :: State (Int, Int) ()
moveSouth = do
    (x, y) <- get
    put (x, y-1)

moveEast :: State (Int, Int) ()
moveEast = do
    (x, y) <- get
    put (x+1, y)

moveWest :: State (Int, Int) ()
moveWest = do
    (x, y) <- get
    put (x-1, y)

repeatState :: Int -> State s () -> State s () --Repeat stateful action N times
repeatState 0 _ = return ()
repeatState n s = do
    s
    repeatState (n-1) s
    