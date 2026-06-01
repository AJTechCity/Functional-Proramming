--Exercise 1 - define a `halve` function which splits an even-length list in half
halve :: [a] -> ([a], [a])
halve xs =
    let half = (length xs) `div` 2
    in
        (take half xs, drop half xs)

--Exercise 2 - Define a `third` function which returns 3rd element in a list that contains at least this many elements
-- Part a) Using `head` and `tail`
third1 :: [a] -> a
-- third1 xs = head (tail (tail xs))
third1 = head . tail . tail

--Part b) Using List Indexing `!!`
third2 :: [a] -> a
third2 xs = xs!!2

-- Part c) Using pattern matching
third3 :: [a] -> a
third3 (_: _: x: _) = x

--Exercise 3 - Consider a `safeTail` function that behaves like `tail` but it maps an empty list to itself rather than causing an error
-- Part a) Using conditional expression + `tail`
safeTail1 :: [a] -> [a]
safeTail1 xs = if (null xs) then [] else tail xs

--Part b) Using guarded equations
safeTail2 :: [a] -> [a]
safeTail2 xs
    | null xs = []
    | otherwise = tail xs

-- Part c) Using pattern matching
safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs

--Exercise 4 - Show how disjunction operator (||) can be defined in 4 ways using pattern matching
(|||) :: Bool -> Bool -> Bool
False ||| a = a
_ ||| _ = True

--Exercise 8 - Create function `luhnDouble` which doubles a digit and subtracts 9 if it is greater than 9
luhnDouble :: Int -> Int
luhnDouble n = if 2*n > 9 then 2*n-9 else 2*n
