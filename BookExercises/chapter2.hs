--Exercise 2 - Parenthesise the following numeric expressions as you'd like

-- 1) 2^3*4
-- (2^3) * 4

-- 2) 2*3+4*5
-- (2*3) + (4*5)

-- 3) 2+3*4^5
-- (2+3) * (4^5)

--Exercise 4 - Define the `last` function - selects the last element of a non-empty list - Give a few examples
myLast :: [a] -> a
myLast = head . reverse
-- myLast xs = head (reverse xs)
-- myLast xs = xs !! (length xs - 1)

--Exercise 5 - `init` removes the last element of an array - show how it can be defined in 2 ways
myInit :: [a] -> [a]
myInit = reverse . tail . reverse
-- myInit xs = reverse (tail (reverse xs))
-- myInit xs = take (length xs - 1) xs