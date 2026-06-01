module Main where
import Code

main = do
  putStrLn "Q1 Tests"
  print (countLeaves (Leaf 1) == 1)
  print (countLeaves (Branch [Leaf 1, Leaf 2]) == 2)
  print (countLeaves (Branch [Leaf 1, Branch [Leaf 2, Leaf 3]]) == 3)

  putStrLn "Q4 Tests"
  print (columnSums [[1,2],[3,4]] == [4,6])

  putStrLn "Q5 Tests"
  print (eval (Add (Val 2) (Val 3)) == 5)

-- Import your solution module above and replace as needed.
