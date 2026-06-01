--Exercise 1 - How does the recurisve factorial function behave for negative inputs
--Before running prediction - It go into an infinite recursion loop since fac (-1) would calculate fac (-2) and then fac (-3) and they would need further calculations. Since the base cases are n=0 and n=1, the argument to fac will never be 0 or 1 since it gets more negative. Hence infinite loop
--After running, modify fac to ensure this error doesn't happen

fac :: (Num a, Eq a, Ord a) => a -> a
fac 0 = 1
fac n
    | n < 0 = error "N must be positive"
    |otherwise = n * fac (n-1)

--Exercise 2 - Define `sunDown` that returns sum of non-negative integers from a given value down to zero
sunDown :: Int -> Int
sunDown 0 = 0
sunDown n = n + sunDown (n-1)

--Exercise 3 - Define exponent operator (^) for non-negative numbers using the same recursion pattern as multiply operator (*)
(^^^) :: Int -> Int -> Int
(^^^) x 0 = 1
(^^^) x n = x * ((^^^) x (n-1))

--Exercise 4 - Define `euclid` that calculates the greatest common divisor of 2 non-negative integers. If the 2 numbers are equal, the input is the result, otherwise, smaller number is subtracted from larger and the process is repeated
euclid :: Int -> Int -> Int
euclid x y
    | x==y = x
    | x > y = euclid (x-y) y
    | otherwise = euclid x (y-x)

--Exercise 5 - Use recursion to define `length`, `drop`, and `init`
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myDrop :: Int -> [a] -> [a]
myDrop _ [] = []
myDrop 0 xs = xs
myDrop num (x:xs) = myDrop (num-1) xs

myInit :: [a] -> [a]
myInit [] = []
myInit [x] = []
myInit (x:xs) = [x] ++ myInit xs

--Exercise 6 - Define the following library functions using recursion
--1) and :: [Bool] -> Bool
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs

--2) concat :: [[a]] -> [a]
myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat [[x]] = [x]
myConcat (xs:xss) = xs ++ myConcat xss

--3) replicate :: Int -> a -> [a]
myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate num x = [x] ++ myReplicate (num-1) x

--4) (!!) :: [a] -> Int -> a
(!!!) :: [a] -> Int -> a
(!!!) (x:xs) 0 = x
(!!!) (x:xs) i = (!!!) xs (i-1)

--5) elem :: Eq a => a -> [a] -> Bool
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem y (x:xs) 
    | y == x = True
    | otherwise = myElem y xs

--Exercise 7 - Define `merge` to merge 2 sorted lists with recursion
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | y < x = y:merge (x:xs) ys
    | otherwise = x:merge xs (y:ys)

--Exercise 8 - Use the above `merge` function to implement `mergeSort` in which empty list and singleton list are already sorted
halve :: [a] -> ([a], [a])
halve xs  = (take (length xs `div` 2) xs, drop (length xs `div` 2) xs)

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort list = 
    let (xs, ys) = halve list
        xs' = mergeSort xs
        ys' = mergeSort ys
    in
        merge xs' ys'