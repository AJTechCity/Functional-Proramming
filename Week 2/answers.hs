orB :: Bool -> Bool -> Bool
-- orB only outputs false when Both inputs are false, all other inputs result in True. Therefore just make a base-case of the (False, False) input and return false, all other inputs return True
orB False False = False
orB _ _ = True

swap :: (a, b) -> (b, a)
-- Return the swapped elements simply
swap (x, y) = (y,x)

removeFirstAndLast :: [a] -> [a]
-- Function to remove the first and last element of a list
removeFirstAndLast list = reverse(tail (reverse (tail list)))