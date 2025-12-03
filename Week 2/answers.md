## Types
Translate the following into Haskell Type Expressions
1) Pairs consisting of a list of integers and string -> ([Int], String)
2) List of functions from an arbitrary type to booleans -> [a-> Bool]
3) Functions from Pairs of strings to lists of Booleans -> (String, String) -> [Bool]
4) Function taking a predicate on integers to an integer (where a predicate on a type `a` is a function from `a` to the booleans) -> (Int->Bool) -> Int
5) Function taking a function from an arbitrary type to itself and returning a function from that same type to itself -> (a->a) -> (a->a)