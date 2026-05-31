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