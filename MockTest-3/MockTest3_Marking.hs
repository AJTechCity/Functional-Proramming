module Main where

main = do
  print (flatten (Branch [Leaf 1, Leaf 2]) == [1,2])
  print (balanced "()()")
  print (not (balanced "(()"))
