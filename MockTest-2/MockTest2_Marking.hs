module Main where

main = do
  print (height Empty == 0)
  print (height (Node 1 Empty Empty) == 1)
  print (chunks 3 [1..8] == [[1,2,3],[4,5,6],[7,8]])
