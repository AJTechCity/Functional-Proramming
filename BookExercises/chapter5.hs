--Exercise 1 - Using list comprehension, give expression which calculates sum of 1^2 + 2^2 ... + 100^2 (first 100 squares)
firstHundreadSquareSum :: Int
firstHundreadSquareSum = sum [x^2 | x<-[1..100]]

--Exercise 2 - Define function `grid` which given m and n returns a coordinate grid of all possible coordinate pairs
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x<-[0..m], y<-[0..n]]

--Exercise 3 - Using list comprehension and the above `grid` function, define function `square` which returns a coordinate square of size n, exluding the leading diagonal
square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x/=y]

--Exercise 4 - Define the library function `replicate` using list comprehension
myReplicate :: Int -> a -> [a]
myReplicate num x = [x | i<-[1..num]]

--Exercise 5 - Define `pyths` which uses list comprehension to generate a list of triples (x, y, z) that are pythagorean
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y<-[1..n], z<-[1..n], x^2 + y^2 == z^2]

--Exercise 6 - Positive int is perfect if it equals sum of all its factors. Use list comprehension and the `factors` function to define `perfects`
factors :: Int -> [Int]
factors num = [f | f<-[1..num], num `mod` f == 0, f/=num]

perfects :: Int -> [Int]
perfects n = [x | x<-[1..n], sum (factors x) == x]

--Exercise 9 - Scalar product of 2 lists xs and ys is the sum of products of corresponding integers - Assume length xs and ys is identical
scalarProduct :: Num a => [a] -> [a] -> a
scalarProduct xs ys = sum [xs!!i*ys!!i | i <- [0..length xs-1]]