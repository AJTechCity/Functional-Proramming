--Exercise 1 - Define instance of functor class for this BT
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

instance Functor Tree where
    fmap g (Leaf a) = Leaf (g a)
    fmap g (Node l a r) = Node (fmap g l) (g a) (fmap g r)

--Exercise 2 - turn partially applied functor (a->) into a functor:
instance Functor ((->) a) where
    fmap g ()