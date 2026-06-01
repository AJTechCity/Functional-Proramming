# Functional Programming Mock Test 1

Time: 90 Minutes
Marks: 50

## Question 1 (10 marks) - Rose Trees

```haskell
data Rose a = Leaf a | Branch [Rose a]
```

Implement:

```haskell
countLeaves :: Rose a -> Int
```

Return the total number of leaves.

Examples:

```haskell
countLeaves (Leaf 5) == 1
countLeaves (Branch [Leaf 1, Leaf 2]) == 2
countLeaves (Branch [Leaf 1, Branch [Leaf 2, Leaf 3]]) == 3
```

## Question 2 (10 marks) - Monadic Recursion

Implement:

```haskell
applyNTimes :: Monad m => m a -> (a -> m a) -> Int -> m [a]
```

Apply the function exactly n times and return all intermediate values.

## Question 3 (10 marks) - State Monad

Implement:

```haskell
tick :: State Int Int
```

Return current state then increment it.

## Question 4 (10 marks) - Matrix Processing

Implement:

```haskell
columnSums :: [[Int]] -> [Int]
```

Return the sum of every column.

## Question 5 (10 marks) - Expression Trees

```haskell
data Expr = Val Int | Add Expr Expr | Mul Expr Expr
```

Implement:

```haskell
eval :: Expr -> Int
```
