--Binary Search Trees
--Exercise 1 - reWrite isBST so tat is runs in Linear time without producing
--the in-order traversal list as an intermediate step
-- isBST :: Ord a => BT a -> Bool


--Rose Trees
data Rose a = Branch a [Rose a] deriving Show
type Direction = Int
type Address = [Direction]

--Exercise 1 - write function which returns all valid addresses in a given tree
validAddresses :: Rose a -> [Address]
validAddresses (Branch _ []) = [[]]
validAddresses (Branch _ children) =
    [] :
    [ i : addr
    | (i, child) <- zip [0..] children
    , addr <- validAddresses child
    ]

--Exercise 2 - Write a function which returns the value found at the node specified by the given address provided that is valid
getAtAddress :: Rose a -> Address -> Maybe a
getAtAddress (Branch x _) [] = Just x
getAtAddress (Branch _ xs) (i:is) = 
    let --Get next direction of address
        childCount = length xs
    in
        if i < 0 || i > childCount-1 then Nothing else getAtAddress (xs!!i) is

------------------------------------

--Exercise 1 - Applying Functions to Trees
data Tree a b = Leaf b | Fork (Tree a b) a (Tree a b) deriving (Eq, Show)

--Implementation Task - Write higher-order func that takes two functions as well as a Tree a b element as input and applies the first function to values at the forks and second function to the values at the leaves.
applyfuns :: (a->c) -> (b->d) -> Tree a b -> Tree c d
applyfuns _ g (Leaf x) = Leaf (g x)
applyfuns f g (Fork l a r ) = Fork (applyfuns f g l) (f a) (applyfuns f g r)

--Exercise 2 - Updating Nodes along a route
data BinTree a = Empty | Node (BinTree a) a (BinTree a) deriving (Eq, Show)

data BinDirection = GoLeft | GoRight deriving (Eq, Show, Bounded, Enum)
type Route = [BinDirection]

--Implementation Task = Implement a function which applies a function to the values of all nodes along a given route in a tree
updateNodes :: Route -> (a -> a) -> BinTree a -> BinTree a
updateNodes _ _  Empty = Empty
updateNodes [] f (Node l x r) = Node l (f x) r
updateNodes (GoLeft : route) f (Node left x right) = Node (updateNodes route f left) (f x) right
updateNodes (GoRight: route) f (Node left x right) = (Node left (f x) (updateNodes route f right))