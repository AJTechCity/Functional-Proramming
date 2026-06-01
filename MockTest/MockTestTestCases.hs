{-# LANGUAGE StandaloneDeriving, FlexibleInstances, DeriveGeneric #-}

module MockTestTestCases where

import Types
import TestCasesUtils
import qualified MockTest as Student
import qualified MockTestSolutions as Solutions

import Test.QuickCheck
import Test.QuickCheck.Monadic

import Control.Monad (replicateM)
import Control.Monad.Writer
import Control.DeepSeq

import Data.List
import Data.Maybe (catMaybes)

import GHC.Generics


deriving instance Generic (Rose a)
instance (NFData a) => NFData (Rose a)
deriving instance (Eq a) => Eq (Rose a)


--------------------------------------------------------------------------------
-- Utilities for string formatting
--------------------------------------------------------------------------------

singleQuotes :: String -> String
singleQuotes s = "'" ++ s ++ "'"

indented :: String -> String
indented s = "\n\n    " ++ s ++ "\n\n"

-------------------------------------------------------------------------------
-- Tests: Question 1
-------------------------------------------------------------------------------

-- gen a Rose tree with exactly N branches of height at most m
genNBranching :: (Eq a, Arbitrary a) => Int -> Gen (Rose a)
genNBranching n = sized $ \m -> do
    k <- chooseInt (0, m)
    go k n where
      go 0 n = Leaf <$> (arbitrary)
      go k n = do
        k' <- chooseInt (0, k - 1)
        Branch <$> replicateM n (go k' n)

gen_not_NBranching :: (Eq a, Arbitrary a) => Int -> Gen (Rose a)
gen_not_NBranching n =  sized $ \m -> do
    k <- chooseInt (0, m)
    go k n -- `suchThat` (not . Solutions.isNBranching n) where ;; this takes too long
     where
      go 0 n = Leaf <$> (arbitrary)
      go k n = do
        k' <- chooseInt (0, k - 1)
        n' <- chooseInt (0, maxBranches)
        Branch <$> replicateM n' (go k' n)


maxBranches :: Int
maxBranches = 9

genLess10 :: Gen Int
genLess10 = chooseInt (0, maxBranches)

subtrees :: Rose a -> [Rose a]
subtrees l@(Leaf a) = [l]
subtrees b@(Branch xs) = b : concat (map subtrees xs)

treeSize :: Rose a -> Int
treeSize (Leaf a) = 1
treeSize (Branch xs) = sum (map treeSize xs) + 1

genRoseBaseCase :: (Arbitrary a) => Gen (Int, Rose a)
genRoseBaseCase = do
       n <- genLess10
       t <- Leaf <$> arbitrary
       return (n, t)

genRoseRecursiveCase :: (Eq a, Arbitrary a) => Gen (Int, Rose a)
genRoseRecursiveCase = do
        h <- chooseInt (3, 4)
        resize h $ do
         n <- genLess10
         t <- replicateM n (genNBranching n)
         return (n, Branch t)



genHelperNBranchingTrue :: (Eq a, Arbitrary a) => Gen (Int, Rose a)
genHelperNBranchingTrue = do
       h <- chooseInt (5, 10)
       resize h $ do
        n <- genLess10
        t <- genNBranching n
        return (n, t)


genHelperNBranching_arbitrary :: (Eq a, Arbitrary a) => Gen (Int, Rose a)
genHelperNBranching_arbitrary = do
       h <- chooseInt (5, 10)
       resize h $ do
        n <- genLess10
        t <- gen_not_NBranching n -- this can happen to be n-branching
        return (n, t)

shrinkNandTreeTrue :: (Eq a) => (Int, Rose a) -> [(Int, Rose a)]
shrinkNandTreeTrue (n,t) = map (\x -> (n,x)) . sortOn treeSize . filter (Solutions.isNBranching n) $ (subtrees t)

shrinkNandTreeFalse :: (Eq a) => (Int, Rose a) -> [(Int, Rose a)]
shrinkNandTreeFalse (n,t) = map (\x -> (n,x)) . sortOn treeSize . filter (not . Solutions.isNBranching n) $ (subtrees t)

shrinkPrune :: (Eq a) => (Int, Rose a) -> [(Int, Rose a)]
shrinkPrune (n,t) = map (\x -> (n,x)) . sortOn treeSize $ subtrees t


shrinkRecursive :: (Eq a) => (Int, Rose a) -> [(Int, Rose a)]
shrinkRecursive (n,t) = map (\x -> (n,x)) . filter (\x -> case x of {Leaf _ -> False; Branch _ -> True}) . sortOn treeSize $ subtrees t

prop_NBranchingTrue :: (Int, Rose Int) -> Property
prop_NBranchingTrue (n,t) = let stu = Student.isNBranching n t in
                           let counterexampleMsg = "The tree\n"
                                                   ++ show t
                                                   ++ "\nis "
                                                   ++ show n
                                                   ++ "-branching, but your answer is\n"
                                                   ++ show stu in
                             counterexample (counterexampleMsg) $ Student.isNBranching n t ~= True

prop_NBranchingFalse :: (Int, Rose Int) -> Property
prop_NBranchingFalse (n,t) = let stu = Student.isNBranching n t in
                            let counterexampleMsg = "The tree\n"
                                                    ++ show t
                                                    ++ "\nis NOT "
                                                    ++ show n
                                                    ++ "-branching, but your answer is\n"
                                                    ++ show stu in
                             counterexample (counterexampleMsg) $ (not . Solutions.isNBranching n $ t) ==> Student.isNBranching n t ~= False


prop_Prune_base :: (Int, Rose Int) -> Property
prop_Prune_base (n, t@(Leaf x)) = (Student.prune n t) ~= t

prop_PruneNBranching ::  (Int, Rose Int) -> Property
prop_PruneNBranching (n,t) = let stu = Student.prune (n-1) t in
                              let counterexampleMsg = "Prunning the " ++ show n ++ " branching tree\n"
                                                      ++ show t
                                                      ++ "\nto "
                                                      ++ show (n-1)
                                                      ++ " should return an " ++ show (n-1) ++ "-branching tree, but your tree is\n"
                                                      ++ show stu
                                                      ++ "\n which is not " ++ show (n-1) ++ "-branching." in
               counterexample counterexampleMsg $ (n > 0) ==> (Solutions.isNBranching (n-1) (Student.prune (n-1) t)) ~= True

prop_Prune_agree ::  (Int, Rose Int) -> Property
prop_Prune_agree (n,t) = let stu = Student.prune n t in
                          let sol = Solutions.prune n t in
                            let counterexampleMsg = "Prunning the tree\n"
                                                    ++ show t
                                                    ++ "\nto "
                                                    ++ show n
                                                    ++ " branches, should return\n"
                                                    ++ show sol
                                                    ++ ", but your answer is\n"
                                                    ++ show stu in
                              counterexample counterexampleMsg $ stu ~= sol

prop_NBranching_base :: (Int, Rose Int) -> Property
prop_NBranching_base (n,t) = let stu = Student.isNBranching n t in
                                let counterexampleMsg = "The base case for N-branching is "
                                                        ++ show t
                                                        ++ " with n=" ++ show n
                                                        ++ " should be True, but your output is "
                                                        ++ show stu in
                                  counterexample counterexampleMsg $ stu ~= True


-- this one might be confusing as it may expect False for an N-branching tree, e.g. id=335811
prop_NBranching_recursive :: (Int, Rose Int) -> Property
prop_NBranching_recursive (n, t@(Branch xs)) = let stu = Student.isNBranching n t in
                                                 stu ~= ((length xs == n) && all (Student.isNBranching n) xs)


--------------------------------------------------------------------------------
-- Tests: Question 2
--------------------------------------------------------------------------------

newtype Q2Wrapper = Wrap (Writer [Int] Int)

-- This is very hacky but simply want to improve printed messages
instance Show Q2Wrapper where
 show (Wrap mx) = let n = fst $ runWriter mx
                   in "do { tell [" ++ show n ++ "] ; pure " ++ show n ++ " } "

q2_mf :: Int -> Writer [Int] Int
q2_mf n = do
  tell [2 * n]
  pure (n + 1)

genWriter :: Gen (Q2Wrapper)
genWriter = do
  n <- chooseInt (0, 15)
  pure $ Wrap $ do { tell [n] ; pure n }

prop_applyNTimes_baseCase :: Q2Wrapper -> Property
prop_applyNTimes_baseCase (Wrap mx) = student ~= solution
 where
  student  = runWriter $ Student.applyNTimes mx q2_mf 0
  solution = runWriter $ (:[]) <$> mx

prop_applyNTimes_recursiveCase :: Q2Wrapper -> Int -> Property
prop_applyNTimes_recursiveCase (Wrap mx) n = counterexample
  ("Your output was " ++ show student' ++ " but based on the value of your " ++
   "recursive call we expected " ++ show solution' ++ ".")
  (student' == solution')
 where
   student    = runWriter $ Student.applyNTimes mx q2_mf n
   studentRec = runWriter $ Student.applyNTimes mx q2_mf (n - 1)
   solution   = runWriter $ do xs <- Student.applyNTimes mx q2_mf (n - 1)
                               x  <- q2_mf (last xs)
                               pure $ xs ++ [x]

   student'    = deepseq student student
   solution'   = deepseq solution solution

prop_applyNTimes_outOfOrder :: Q2Wrapper -> Int -> Property
prop_applyNTimes_outOfOrder (Wrap mx) n = p_permOf student solution
 where
   student  = fst $ runWriter $ Student.applyNTimes mx q2_mf n
   solution = fst $ runWriter $ Solutions.applyNTimes mx q2_mf n

prop_applyNTimes_correct :: Q2Wrapper -> Int -> Property
prop_applyNTimes_correct (Wrap mx) n = student ~= solution
 where
  student  = runWriter $ Student.applyNTimes mx q2_mf n
  solution = runWriter $ Solutions.applyNTimes mx q2_mf n

-------------------------------------------------------------------------------
-- Tests: Question 3
-------------------------------------------------------------------------------

genBoardWithChosenHeapEmpty :: Heap -> Gen NimBoard
genBoardWithChosenHeapEmpty h = do
  k <- chooseInt (1, 35)
  let f = case h of { First -> (,); Second -> flip (,) }
  return $ f 0 k

genBoardWithNeitherHeapEmpty :: Gen NimBoard
genBoardWithNeitherHeapEmpty = do
  m <- chooseInt (1, 35)
  n <- chooseInt (1, 35)
  return (m, n)

genIntegerBiasedTowardsZero :: Gen Int
genIntegerBiasedTowardsZero =
  frequency [(4, return 0), (16, chooseInt (0, 35))]

genBoardWithOneHeapPossiblyEmpty :: Gen NimBoard
genBoardWithOneHeapPossiblyEmpty = do
  m <- genIntegerBiasedTowardsZero
  n <- if m == 0 then chooseInt (1, 35) else genIntegerBiasedTowardsZero
                 -- Avoids the case where both heaps are empty.
  return (m, n)

instance Arbitrary Heap where

  arbitrary :: Gen Heap
  arbitrary = elements [First, Second]

deriving instance Show Heap

prop_gameOverComplete :: Property
prop_gameOverComplete =
  let
    (p, _) = runState Student.gameOver (0, 0)
  in
    p ~= True

prop_gameOverSound :: NimBoard -> Property
prop_gameOverSound b =
  let
    (p, _) = runState Student.gameOver b
  in
    p ~= False

-- Generates a triple `(b, n, h)` where `b` is a board, with neither heap
-- nonempty and whose heap `h` has less than `n` elements.
genTripleNumGreater :: Gen (NimBoard, Int, Heap)
genTripleNumGreater = do
  (h1, h2) <- genBoardWithNeitherHeapEmpty
  h <- arbitrary
  k <- chooseInt (1, 10)
  let n = case h of { First -> h1+k ; Second -> h2+k }
  return ((h1, h2), n, h)

-- Generates a triple `(b, n, h)` where `b` is a board, with neither heap
-- nonempty, and whose heap `h` has at least `n` elements.
genTripleNumLTE :: Gen (NimBoard, Int, Heap)
genTripleNumLTE = do
  (h1, h2) <- genBoardWithNeitherHeapEmpty
  h <- arbitrary
  let k = case h of { First -> h1 ; Second -> h2 }
  n <- chooseInt (1, k)
  return ((h1, h2), n, h)

-- Property asserting that `Student.takeTokens` gives the same result
-- as `Solutions.takeTokens`.
prop_takeTokens :: (NimBoard, Int, Heap) -> Property
prop_takeTokens (b@(h1, h2), k, h) =
  let
    ((), boardStu') = runState (Student.takeTokens k h) b
    boardStu        = deepseq boardStu' boardStu'
    ((), boardSol)  = runState (Solutions.takeTokens k h) b
    msg             = unwords
                        [ "Running 'takeTokens " ++ show h ++ " " ++ show k ++ "'"
                        , "on the board '" ++ show b ++ "'"
                        , "gave '" ++ show boardStu ++ "'"
                        , "but the expected result was:"
                        , show boardSol
                        ]
  in
    if boardStu == boardSol then
      property True
    else
      counterexample msg False

-------------------------------------------------------------------------------
-- Tests: Question 4:
-------------------------------------------------------------------------------

genSquare :: Int -> Gen [[Int]]
genSquare n = replicateM n (replicateM n arbitrary)

genSingleSquare :: Gen [[Int]]
genSingleSquare = genSquare 1

genNonMagicSquare :: Gen [[Int]]
genNonMagicSquare = do
                      size <- chooseInt (2, 5)
                      genSquare size `suchThat` \ x -> not $ Solutions.isMagicSquare x

magic3x3 :: [[Int]]
magic3x3 =
  [ [8, 1, 6]
  , [3, 5, 7]
  , [4, 9, 2]
  ]

magic4x4 :: [[Int]]
magic4x4 =
  [ [ 1, 15, 14,  4]
  , [12,  6,  7,  9]
  , [ 8, 10, 11,  5]
  , [13,  3,  2, 16]
  ]

magic5x5 :: [[Int]]
magic5x5 =
  [ [17, 24,  1,  8, 15]
  , [23,  5,  7, 14, 16]
  , [ 4,  6, 13, 20, 22]
  , [10, 12, 19, 21,  3]
  , [11, 18, 25,  2,  9]
  ]

prop_magicSquaresTrue :: Property
prop_magicSquaresTrue = (map Student.isMagicSquare squares) ~= (map Solutions.isMagicSquare squares)
  where 
    squares = [magic3x3, magic4x4, magic5x5]

prop_magicSquare :: [[Int]] -> Property
prop_magicSquare s = Student.isMagicSquare s ~= Solutions.isMagicSquare s

isSemiMagic :: [[Int]] -> Bool
isSemiMagic xss = all (== sum (head xss)) allsums
 where
  rowsums    = map sum xss
  columnsums = map sum (transpose xss)
  allsums    = rowsums ++ columnsums

prop_semiMagicTrue :: [[Int]] -> Property
prop_semiMagicTrue s = Student.isMagicSquare s ~= isSemiMagic s

-------------------------------------------------------------------------------
-- Tests: Question 5
-------------------------------------------------------------------------------

instance NFData Expr where

  rnf :: Expr -> ()
  rnf (Var     x)     = rnf x
  rnf (Not     e)     = rnf e
  rnf (And     e0 e1) = rnf e0 `seq` rnf e1
  rnf (Or      e0 e1) = rnf e0 `seq` rnf e1
  rnf (Implies e0 e1) = rnf e0 `seq` rnf e1

instance NFData Circuit where

  rnf :: Circuit -> ()
  rnf (Input x)    = rnf x
  rnf (Nand c0 c1) = rnf c0 `seq` rnf c1

arbitraryVar :: Gen Expr
arbitraryVar = do
  s <- elements ['a'..'f']
  return $ Var s

arbitraryVarPair :: Gen (Expr, Expr)
arbitraryVarPair = do
  e1 <- arbitraryVar
  e2 <- arbitraryVar
  return (e1 , e2)

sizedArbitraryExpr :: Int -> Gen Expr
sizedArbitraryExpr 0 = arbitraryVar
sizedArbitraryExpr n =
  let
    genNeg = do
      t <- sizedArbitraryExpr (n - 1)
      return (Not t)

    genCon :: (Expr -> Expr -> Expr) -> Gen Expr
    genCon c = do
      s <- sizedArbitraryExpr (n `div` 2)
      t <- sizedArbitraryExpr (n `div` 2)
      return (c s t)
  in
    frequency [ (3, arbitraryVar)
              , (2, genCon And)
              , (2, genCon Or)
              , (2, genCon Implies)
              , (2, genNeg) ]

instance Arbitrary Expr where

  arbitrary :: Gen Expr
  arbitrary = do
    n <- elements [1..7]
    sizedArbitraryExpr n

  shrink :: Expr -> [Expr]
  shrink (Var _)         = []
  shrink (Not e)         = e : shrink e
  shrink (And    e0 e1)  = e0 : e1 : [And e0' e1' | (e0' , e1') <- ss ]
                             where
                               ss = shrink (e0, e1)
  shrink (Or     e0 e1)  = e0 : e1 : [Or e0' e1' | (e0' , e1') <- ss ]
                             where
                               ss = shrink (e0, e1)
  shrink (Implies e0 e1) = e0 : e1 : [Implies e0' e1' | (e0' , e1') <- ss ]
                             where
                               ss = shrink (e0, e1)

-- The list of all variables mentioned in an expression.
vars :: Expr -> [Char]
vars (Var     c)     = [c]
vars (Not     e)     = vars e
vars (And     e1 e2) = vars e1 ++ vars e2
vars (Or      e1 e2) = vars e1 ++ vars e2
vars (Implies e1 e2) = vars e1 ++ vars e2

type Context = [Char]

arbitraryCtx :: Gen Context
arbitraryCtx = do
  l <- elements [1..15]
  vectorOf l (elements ['a'..'f'])

-- The Boolean function denoted by an expression. It maps a context to a
-- Boolean. The presence of a variable in a context means it is true. Absence
-- means it is false.
semExpr :: Expr -> Context -> Bool
semExpr (Var     x)     ctx = x `elem` ctx
semExpr (Not     e)     ctx = not $ semExpr e ctx
semExpr (And     e1 e2) ctx = semExpr e1 ctx && semExpr e2 ctx
semExpr (Or      e1 e2) ctx = semExpr e1 ctx || semExpr e2 ctx
semExpr (Implies e1 e2) ctx = semExpr (Or (Not e1) e2) ctx

-- The nand operation.
nand :: Bool -> Bool -> Bool
nand True True = False
nand _    _    = True

allSublists :: [a] -> [[a]]
allSublists xs = flip take xs <$> [0..length xs]

-- The list of all variables mentioned in a circuit.
inputs :: Circuit -> Context
inputs (Input x)     = [x]
inputs (Nand  c1 c2) = inputs c1 ++ inputs c2

-- Semantics of a circuit. Similar to `semExpr`.
semCirc :: Circuit -> Context -> Bool
semCirc (Input x)     ctx = x `elem` ctx
semCirc (Nand  c1 c2) ctx = nand (semCirc c1 ctx) (semCirc c2 ctx)

-- Tries to find a context refuting the equivalence of a circuit and an expression.
tryToRefuteEquivalence :: Circuit -> Expr -> [Context]
tryToRefuteEquivalence c e =
  catMaybes $ (\ctx -> if semCirc c ctx == semExpr e ctx then Nothing else Just ctx) <$> allSublists is
    where
      is = nub (inputs c ++ vars e)

explicitForm :: [Char] -> Context -> [(Char, Bool)]
explicitForm cs ctx = (\c -> (c, c `elem` ctx)) <$> cs

showCtx :: [Char] -> Context -> String
showCtx cs ctx =
  let
    mapsto s1 s2 = s1 ++ " |-> " ++ s2
    ic = intercalate ", "
  in
    unwords
      [ "{"
      , ic $ (\(c, b) -> show (Var c) `mapsto` show b) <$> explicitForm cs ctx
      , "}"
      ]

-- The following is useful for abstracting over a certain class of properties.

data BinConnective = Conj | Disj | Impl deriving (Eq)

showConn :: BinConnective -> String
showConn Conj = "And"
showConn Disj = "Or"
showConn Impl = "Implies"

vernConn :: BinConnective -> String
vernConn Conj = "conjunction"
vernConn Disj = "disjunction"
vernConn Impl = "implication"

binConnective :: BinConnective -> Expr -> Expr -> Expr
binConnective Conj = And
binConnective Disj = Or
binConnective Impl = Implies

sizedArbitraryExprOnlyNot :: Int -> Gen Expr
sizedArbitraryExprOnlyNot 0 = arbitraryVar
sizedArbitraryExprOnlyNot n = do t <- sizedArbitraryExprOnlyNot (n - 1)
                                 return (Not t)

arbitraryExprOnlyNot :: Gen Expr
arbitraryExprOnlyNot = do
  n <- chooseInt (0, 9)
  sizedArbitraryExprOnlyNot n

sizedArbitraryExprOnlyAnd :: Int -> Gen Expr
sizedArbitraryExprOnlyAnd 0 = arbitraryVar
sizedArbitraryExprOnlyAnd n =
  do e1 <- frequency [ (3, arbitraryVar)
                     , (2, sizedArbitraryExprOnlyAnd (n `div` 2))]
     e2 <- frequency [ (3, arbitraryVar)
                     , (2, sizedArbitraryExprOnlyAnd (n `div` 2))]
     return $ And e1 e2

arbitraryExprOnlyAnd :: Gen Expr
arbitraryExprOnlyAnd = do n <- chooseInt (0, 18)
                          sizedArbitraryExprOnlyAnd n

sizedArbitraryExprFreeOf :: Int -> BinConnective -> Gen Expr
sizedArbitraryExprFreeOf 0 = const arbitraryVar
sizedArbitraryExprFreeOf n = frequency . freqListFreeOf n

freqListFreeOf :: Int -> BinConnective -> [(Int, Gen Expr)]
freqListFreeOf n conn =
  let
    genNeg = do
      t <- sizedArbitraryExprFreeOf (n - 1) conn
      return (Not t)

    genCon :: (Expr -> Expr -> Expr) -> Gen Expr
    genCon c = do
      s <- sizedArbitraryExprFreeOf (n `div` 2) conn
      t <- sizedArbitraryExprFreeOf (n `div` 2) conn
      return (c s t)
  in
    case conn of
      Conj -> [ (3, arbitraryVar) ,  (2, genCon Or)  , (2, genCon Implies) , (2, genNeg) ]
      Disj -> [ (3, arbitraryVar) ,  (2, genCon And) , (2, genCon Implies) , (2, genNeg) ]
      Impl -> [ (3, arbitraryVar) ,  (2, genCon And) , (2, genCon Or)      , (2, genNeg) ]

arbitraryExprFreeOf :: BinConnective -> Gen Expr
arbitraryExprFreeOf conn = do
  n <- chooseInt (0, 18)
  sizedArbitraryExprFreeOf n conn

makeCircuitProperty :: Expr -> Property
makeCircuitProperty e =
  let
    stu' = Student.circuit e
    stu  = deepseq stu' stu'
    cs   = nub $ vars e ++ inputs stu
   in
     case tryToRefuteEquivalence stu e of
       []      -> property True
       (ref:_) -> let
                    msg  = unwords
                             [ "When your 'circuit' function was called on"
                             , singleQuotes $ show e
                             , "it returned a circuit"
                             , "that is not logically equivalent because"
                             , "under the assignment"
                             , indented (showCtx cs ref) ++ "it"
                             , "evaluates to"
                             , singleQuotes (show (semCirc stu ref))
                             , "whereas the expression " ++ singleQuotes (show e)
                             , "evaluates to "
                               ++ singleQuotes (show (semExpr e ref))
                             ]
                  in
                    counterexample msg False
