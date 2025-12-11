--List comprehensions

--Exercise 1

pyths :: Int -> [(Int, Int, Int)]
--Use list comprehensions to generate triples (x,y,z) where they are pythagorean x^2+y^2=z^2
pyths max = [(x,y,z) | x<-[1..max], y<-[1..max], z<-[1..max], (x^2 + y^2) == z^2]

--Exercise 2
isPerfectNum :: Int -> Bool
-- We check if it is equal to 2*num since the sum will include the number itself as a factor
isPerfectNum num = (sum[a | a<-[1..num], num `mod` a == 0]) == 2*num

perfects :: Int -> [Int]
--Return a list of all perfect numbers in the given range - perfect nuumber equals the sum of all its factors excluding the number itself
perfects max = [x | x<-[1..max], isPerfectNum x]

--Exercise 3
scalarProduct :: [Int] -> [Int] -> Int
--Scalar Product of two lists of integers is given by the sum of the products of the corresponding integers
scalarProduct xs ys = sum [x * y | (x,y) <- (zip xs ys)]

--Exercise 4 (Hard)
matrix_mul :: Int -> Int -> Int -> [[Int]] -> [[Int]] -> [[Int]]
--Multiply 2 matrices together using list comprehensions
matrix_mul cols1 cols2 rows2 mat1 mat2= undefined

--Recursive Functions
--Exercise 1
and :: [Bool] -> Bool
and [False] = False
and[True] = True
and (x:xs) = x && Main.and xs

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x:xs) = x ++ myConcat xs

myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate count x = [x] ++ myReplicate (count -1) x

(!!!) :: [a] -> Int -> a
(!!!) list 0 = head list
(!!!) list index = (!!!) (tail list) (index - 1)

myElem :: Eq a => a -> [a] -> Bool
myElem item [] = False
myElem item list | (head list) == item = True
    | otherwise = myElem item (tail list)

--Exercise 2
split :: [a] -> [[a]]
split list = [take (length list `div` 2) list, drop (length list `div`2) list]

merge :: Ord a => [a] -> [a] -> [a]
--Merges two sorted lists of values into a single sorted list
merge l1 l2 = undefined