import Data.Char
import Data.List

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

bigReverse :: [a] -> [a]
-- Reverse a list if it has more than 7 elements, otherwise return it unchanged
bigReverse xs = if (length xs) > 7 then (reverse xs) else xs

bigReverseParam :: [a] -> Int -> [a]
-- Same as above but make the length limit a variable
bigReverseParam xs limit = if (length xs) > limit then (reverse xs) else xs

doubleAll :: [Int] -> [Int]
-- Function to Double all elements of a list l :: [Int] and then keeps only those greater than 10
doubleAll xs = [2*a | a <- xs, a > 10]

reverseUpper :: String -> String
-- Function to reverse a string with all letters capitalised
reverseUpper xs = [toUpper a | a <- reverse xs]

pairElementWithIndex :: [a] -> [(Int, a)]
--Function to pair each element of a list with its index
pairElementWithIndex xs = zip [0..(length xs)] xs

customFunc :: Int -> Int -> Bool
-- Returns True if arg1 > arg 2 AND arg1 < (2*arg2)
customFunc arg1 arg2 | arg2>arg1 = False
    | (2*arg2)<arg1 = False
    | otherwise = True

--Define 3 variants of a function third :: [a] -> a that returns the 
--third element in any list that contains at least 3 elements using 
-- 1) head and tail, 2) list indexing !!, 3) pattern matching

third1 :: [a] -> a
--Using Head and Tail
third1 xs = head (tail (tail xs))

third2 :: [a] -> a
-- Using List indexing !!
third2 xs = xs !! 2

third3 :: [a] -> a
-- Using Pattern matching (Unpack/pattern match list for the first 3 elements)
third3 (_:(_:(x:xs))) = x

-- Function that behaves like tail except it maps [] to [] (instead of an error)
--Use tail and isEmpty :: [a] -> Bool to define safetail using
-- 1) A Conditional Expression
-- 2) Guarded Equations
-- 3) Pattern Matching

--Custom isEmpty Functions
isEmpty :: [a] -> Bool
isEmpty xs = (length xs == 0)

safetail1 :: [a] -> [a]
--Using Conditional Expression
safetail1 xs = if isEmpty xs then [] else tail xs

safetail2 :: [a] -> [a]
--Using Guarded Equations
safetail2 xs | isEmpty xs = []
    | otherwise = tail xs

safetail3 :: [a] -> [a]
--Pattern Matching
safetail3 [] = []
safetail3 xs = tail xs --(Could have done safetail3 (x:xs) = xs as well)