# Functional Programming Mock Test 3

## Question 1 (10 marks)

```haskell
data Rose a = Leaf a | Branch [Rose a]
```

Implement:

```haskell
flatten :: Rose a -> [a]
```

Return all leaves left-to-right.

## Question 2 (10 marks)

```haskell
data Free f a
  = Pure a
  | Free (f (Free f a))
```

Implement:

```haskell
countPure :: Free [] a -> Int
```

Count all Pure constructors.

## Question 3 (10 marks)

Implement:

```haskell
moveNorth :: State (Int,Int) ()
moveSouth :: State (Int,Int) ()
moveEast  :: State (Int,Int) ()
moveWest  :: State (Int,Int) ()
```

## Question 4 (10 marks)

Implement:

```haskell
balanced :: String -> Bool
```

Check if brackets are balanced.

## Question 5 (10 marks)

Implement:

```haskell
repeatState :: Int -> State s () -> State s ()
```
