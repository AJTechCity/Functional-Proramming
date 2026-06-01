--Exercise 2 - Show that sum [x] = x for any number x
longSum :: Num a => [a] -> a
longSum = foldr (\x acc -> acc+x) 0

--If longSum [x] == sum [x] then sum[x] = x for any number x

--Exercise 3 - Define the function product which returns the product of a list of numbers
myProduct :: Num a => [a] -> a
myProduct = foldr (\x acc -> acc*x) 1