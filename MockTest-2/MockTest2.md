# Functional Programming Mock Test 2

## Question 1 (10 marks) - Binary Trees

```haskell
data Tree a = Empty | Node a (Tree a) (Tree a)
```

Implement:

```haskell
height :: Tree a -> Int
mirror :: Tree a -> Tree a
```

height returns maximum depth.
mirror swaps left/right subtrees.

## Question 2 (10 marks)

Implement:

```haskell
safeChain :: (a -> Maybe a) -> Int -> a -> Maybe a
```

Apply the function n times. Any Nothing causes failure.

## Question 3 (10 marks)

Implement:

```haskell
decrement :: State Int ()
decrementN :: Int -> State Int ()
```

## Question 4 (10 marks)

Implement:

```haskell
chunks :: Int -> [a] -> [[a]]
```

Split a list into chunks of size n.

## Question 5 (10 marks)

Implement:

```haskell
simplify :: Expr -> Expr
```

using boolean simplification rules.
