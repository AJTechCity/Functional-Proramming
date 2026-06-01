module MockTestMarking where

import MockTest
import TestCasesUtils
import MockTestTestCases

-- import Test.QuickCheck
import Data.Functor ((<&>))
import Types

env :: TestEnv
env = TestEnv {
  assessed = True,
  maxScore = 100 -- total marks for MockTest
              }

main :: IO ()
main = runMarking' [question1, question2, question3, question4, question5] env

{- Question 1 -}

question1 = Question {name = "Question 1", tests = 
  [ test_NBranching_base
  , test_NBranching_recursive
  , test_NBranchingTrue
  , test_NBranchingFalse
  , test_Prune_base
  , test_PruneNBranching
  , test_Prune_agree
  ]
}

test_NBranching_base :: Test
test_NBranching_base = Test
  { mark = getMarks m
  , description = "Checking that your 'isNBranching' function handles the base case correctly."
  , successMsg = "You got " ++ show m ++ " because your 'isNbranching' handled the base case correctly."
  , failMsg = "Your 'isNBranching' function did not handle the base case correctly."
  , prop = makeUnaryPropWith prop_NBranching_base (genRoseBaseCase) (\x -> [x])
  , condition = Always
  } where
  m = Marks 2


test_NBranching_recursive :: Test
test_NBranching_recursive = Test
  { mark = getMarks m
  , description = "Checking that your 'isNBranching' function handles the recursive case correctly."
  , successMsg = "You got " ++ show m ++ " because your 'isNBranching' handled the recursive case correctly."
  , failMsg = "Your 'isNBranching' did not handle the recursive case correctly."
  , prop = makeUnaryPropWith prop_NBranching_recursive (genRoseRecursiveCase) (shrinkRecursive)
  , condition = IfFail test_NBranching_base
  } where
  m = Marks 2

test_NBranchingTrue :: Test
test_NBranchingTrue = Test
  { mark = getMarks m
  , description = "Checking that an N-branching rose tree is correctly identified as such."
  , successMsg = "You got " ++ show m ++ " because your 'isNbranching' returned True for trees which are N-branching."
  , failMsg = "Your 'isNBranching' function did not correctly identify an N-branching tree."
  , prop = makeUnaryPropWith prop_NBranchingTrue (genHelperNBranchingTrue) shrinkNandTreeTrue
  , condition = IfSuccess test_NBranching_base
  } where
  m = Marks 4



test_NBranchingFalse :: Test
test_NBranchingFalse = Test
  { mark = getMarks m
  , description = "Checking that a non-N-branching rose tree is correctly identified as such."
  , successMsg = "You got " ++ show m ++ " because your 'isNbranching' returned False for trees which are not N-branching."
  , failMsg = "Your 'isNBranching' function did not correctly identify a non-N-branching tree."
  , prop = makeUnaryPropWith prop_NBranchingFalse (genHelperNBranching_arbitrary) shrinkNandTreeFalse
  , condition = IfSuccess test_NBranching_base
  } where
  m = Marks 4

-- 5 marks

test_Prune_base :: Test
test_Prune_base = Test
  { mark = getMarks m
  , description = "Checking that your 'prune' function handles the base case correctly."
  , successMsg = "You got " ++ show m ++ " because your 'prune' function handled the base case correctly."
  , failMsg = "Your 'prune' function did not handle the base case correctly."
  , prop = makeUnaryPropWith prop_Prune_base (genRoseBaseCase) shrinkNandTreeTrue
  , condition = Always
  } where
  m = Marks 2

test_PruneNBranching :: Test
test_PruneNBranching = Test
  { mark = getMarks m
  , description = "Checking that pruning an N-branching rose tree to (N-1) returns an (N-1)-branching tree."
  , successMsg = "You got " ++ show m ++ " because your 'prune' function returned an (N-1)-branching tree when prunning an N-branching tree to (N-1)."
  , failMsg = "Your 'prune' function did not correctly prune an N-branching tree."
  , prop = makeUnaryPropWith prop_PruneNBranching (genHelperNBranchingTrue) shrinkNandTreeTrue
  , condition = Always
  } where
  m = Marks 4

test_Prune_agree :: Test
test_Prune_agree = Test
  { mark = getMarks m
  , description = "Checking that your 'prune' function is correct for arbitrary trees."
  , successMsg = "You got " ++ show m ++ " because your 'prune' function is correct."
  , failMsg = "Your 'prune' function is not correct."
  , prop = makeUnaryPropWith prop_Prune_agree (genHelperNBranching_arbitrary) shrinkPrune
  , condition = Always
  } where
  m = Marks 4

{- Question 2 -}

question2 = Question {name = "Question 2", tests = 
  [ test_applyNTimes_baseCase
  , test_applyNTimes_recursiveCase
  , test_applyNTimes_outOfOrder
  , test_applyNTimes_correct
  ]
}

test_applyNTimes_baseCase :: Test
test_applyNTimes_baseCase =
  let
    marks = Marks 2
    functionName = "'applyNTimes'"
  in Test
  { mark        = getMarks marks
  , description = unwords
                [ "Checking that"
                , functionName
                , "handles the base case correctly by using the writer monad."
                ]
  , successMsg  = unwords
                [ "You got"
                , show marks
                , "because your"
                , functionName
                , "function correctly handled the base case."
                ]
  , failMsg     = unwords
                [ "Your"
                , functionName
                , "function did not correctly handle the base case."
                ]
  , prop        = makeUnaryPropWith
                    prop_applyNTimes_baseCase
                    genWriter
                    (const [])
  , condition   = Always
  }

test_applyNTimes_recursiveCase :: Test
test_applyNTimes_recursiveCase =
  let
    marks = Marks 8
    functionName = "'applyNTimes'"
  in Test
  { mark        = getMarks marks
  , description = unwords
                [ "Checking that"
                , functionName
                , "handles the recursive case correctly by using the writer"
                , "monad and using your function for the recursive call."
                ]
  , successMsg  = unwords
                [ "You got"
                , show marks
                , "because your"
                , functionName
                , "function correctly handled the recursive case."
                ]
  , failMsg     = unwords
                [ "Your"
                , functionName
                , "function did not correctly handle the recursive case."
                ]
  , prop        = makeBinaryPropWith
                    prop_applyNTimes_recursiveCase
                    -- generate from 2 so students that index from 0
                    -- can still get some partial marks
                    ((,) <$> genWriter <*> chooseInt (2, 10))
                    (\(mx, n) -> [ (mx, i) | i <- [ 2 .. (n - 1) ] ])
  , condition   = Always
  }

test_applyNTimes_outOfOrder :: Test
test_applyNTimes_outOfOrder =
  let
    marks = Marks 6
    functionName = "'applyNTimes'"
  in Test
  { mark        = getMarks marks
  , description = unwords
                [ "Checking that"
                , functionName
                , "returns all the intermediate values ignoring their order."
                ]
  , successMsg  = unwords
                [ "You got"
                , show marks
                , "because your"
                , functionName
                , "function returned all the intermediate values."
                ]
  , failMsg     = unwords
                [ "Your"
                , functionName
                , "function did not return all the intermediate values."
                ]
  , prop        = makeBinaryPropWith
                    prop_applyNTimes_outOfOrder
                    ((,) <$> genWriter <*> chooseInt (1, 10))
                    (\(mx, n) -> [ (mx, i) | i <- [ 1 .. n ] ])
  , condition   = Always
  }

test_applyNTimes_correct :: Test
test_applyNTimes_correct =
  let
    marks = Marks 4
    functionName = "'applyNTimes'"
  in Test
  { mark        = getMarks marks
  , description = unwords
                [ "Checking that"
                , functionName
                , "works correctly by using the writer monad."
                ]
  , successMsg  = unwords
                [ "You got"
                , show marks
                , "because your"
                , functionName
                , "function worked correctly."
                ]
  , failMsg     = unwords
                [ "Your"
                , functionName
                , "function did not work correctly."
                ]
  , prop        = makeBinaryPropWith
                    prop_applyNTimes_correct
                    ((,) <$> genWriter <*> chooseInt (1, 10))
                    (\(mx, n) -> [ (mx, i) | i <- [ 1 .. n ] ])
  , condition   = Always
  }

{- Question 3 -}

question3 = Question {name = "Question 3", tests = 
  [ test_gameOverComplete
  , test_gameOverSound
  , test_gameOverSoundBothHeapsNonempty
  , test_takeTokensNumGreater
  , test_takeTokensNumLTE
  ]
}

test_gameOverComplete :: Test
test_gameOverComplete = Test
  { mark        = getMarks m
  , description = unwords
                    [ "Checking that your 'gameOver' function gives 'True'"
                    , "on the Nim board '(0, 0)'."
                    ]
  , successMsg  = unwords
                    [ "You got"
                    , show m
                    , "because your 'gameOver' function correctly"
                    , "returned 'True' on the Nim board '(0, 0)'."
                    ]
  , failMsg     = unwords
                    [ "Your 'gameOver' function did not return 'True' in game"
                    , "states where both heaps are empty."
                    ]
  , prop        = makeNullaryProp prop_gameOverComplete
  , condition   = Always
  }
  where
    m = Marks 4

test_gameOverSound :: Test
test_gameOverSound = Test
  { mark        = getMarks m
  , description = unwords
                    [ "Checking that your 'gameOver' function gives 'False'"
                    , "on nonempty Nim boards."
                    ]
  , successMsg  = unwords
                    [ "You got"
                    , show m
                    , "because your 'gameOver' function correctly"
                    , "returned 'False' on nonempty Nim boards."
                    ]
  , failMsg     = unwords
                    [ "Your 'gameOver' function did not return 'False' on"
                    , "nonempty Nim boards."
                    ]
  , prop        = makeUnaryPropWith
                    prop_gameOverSound
                    genBoardWithOneHeapPossiblyEmpty
                    (const [])
  , condition   = Always
  }
  where
    m = Marks 6

test_gameOverSoundBothHeapsNonempty :: Test
test_gameOverSoundBothHeapsNonempty = Test
  { mark        = getMarks m
  , description = unwords
                    [ "Checking that your 'gameOver' function gives 'False'"
                    , "on Nim boards where neither heap is empty."
                    ]
  , successMsg  = unwords
                    [ "You got"
                    , show m
                    , "because your 'gameOver' function correctly"
                    , "returned 'False' on boards where neither heap is empty."
                    ]
  , failMsg     = unwords
                    [ "Your 'gameOver' function did not return 'False' on"
                    , "Nim boards where neither heap is empty."
                    ]
  , prop        = makeUnaryPropWith
                    prop_gameOverSound
                    genBoardWithNeitherHeapEmpty
                    (const [])
  , condition   = IfFail test_gameOverSound
  }
  where
    m = Marks 2

test_takeTokensNumGreater :: Test
test_takeTokensNumGreater = Test
  { mark        = getMarks m
  , description = unwords
                    [ "Checking that your 'takeTokens' function works correctly"
                    , "in cases where the number of tokens to be taken"
                    , "is greater than the number of tokens present"
                    , "in the chosen heap."
                    ]
  , successMsg  = unwords
                    [ "You got"
                    , show m
                    , "because your 'takeTokens' function worked correctly"
                    , "in cases where the number of tokens to be taken"
                    , "is greater than the number of tokens present"
                    , "in the chosen heap."
                    ]
  , failMsg     = unwords
                    [ "Your 'takeTokens' function did not work correctly"
                    , "in cases where the number of tokens to be taken"
                    , "is greater than the number of tokens present"
                    , "in the chosen heap."
                    ]
  , prop        = makeUnaryPropWith
                    prop_takeTokens
                    genTripleNumGreater
                    (const [])
  , condition   = Always
  }
  where
    m = Marks 4

test_takeTokensNumLTE :: Test
test_takeTokensNumLTE = Test
  { mark        = getMarks m
  , description = unwords
                    [ "Checking that your 'takeTokens' function modifies"
                    , "the chosen heap correctly"
                    , "in cases where the number of tokens to be taken"
                    , "is less than or equal to the"
                    , "number of tokens present in the chosen heap."
                    ]
  , successMsg  = unwords
                    [ "You got"
                    , show m
                    , "because your 'takeTokens' function"
                    , "modified the heap correctly in cases where the"
                    , "number of tokens to be taken is less than or equal to"
                    , "the number of tokens present in the chosen heap."
                    ]
  , failMsg     = unwords
                    [ "Your 'takeTokens' function did not modify"
                    , "the heap correctly"
                    , "in a case where"
                    , "the number of tokens to be taken"
                    , "is less than or equal to"
                    , "the number of tokens in the chosen heap"
                    ]
  , prop        = makeUnaryPropWith
                    prop_takeTokens
                    genTripleNumLTE
                    (const [])
  , condition   = Always
  }
  where
    m = Marks 6

{- Question 4 -}

question4 = Question {name = "Question 4", tests = 
  [ test_trueWhenMagic
  , test_falseWhenNotMagic
  , test_trueWhenMagicRowsCols
  , test_singleSquare
  ]
}

test_trueWhenMagic :: Test
test_trueWhenMagic = Test
  { mark        = getMarks m
  , description = newSection ++ "Checking that your implementation of `isMagicSquare`\
                  \ correctly returns `True` for magic squares..."
  , successMsg  = "You got " ++ show m ++ " because your implementation of `isMagicSquare`\
                  \ correctly identifies magic squares"
  , failMsg     = "Your implementation of `isMagicSquare` did not\
                  \ correctly identify magic squares"
  , prop        = makeNullaryProp prop_magicSquaresTrue
  , condition   = Always
  }
  where 
    m = Marks 8

test_falseWhenNotMagic :: Test
test_falseWhenNotMagic = Test
  { mark        = getMarks m
  , description = "Checking that your implementation of `isMagicSquare`\
                  \ correctly returns `False` for non-magic squares..."
  , successMsg  = "You got " ++ show m ++ " because your implementation of `isMagicSquare`\
                  \ correctly identifies non-magic squares"
  , failMsg     = "Your implementation of `isMagicSquare` did not\
                  \ correctly identify non-magic squares"
  , prop        = makeUnaryPropWith
                    prop_magicSquare
                    genNonMagicSquare
                    (const [])
  , condition   = Always
  }
  where 
    m = Marks 8

test_singleSquare :: Test
test_singleSquare = Test
  { mark        = getMarks m
  , description = "Checking that your implementation of `isMagicSquare`\
                  \ returns `True` for squares with only one value..."
  , successMsg  = "You got " ++ show m ++ " because your implementation of `isMagicSquare`\
                  \ returns `True` for squares with only one value"
  , failMsg     = "Your implementation did not return `True` for squares with\
                  \ only one value"
  , prop        = makeUnaryPropWith
                    prop_magicSquare
                    genSingleSquare
                    (const [])
  , condition   = Always
  }
  where 
    m = Marks 4

test_trueWhenMagicRowsCols :: Test
test_trueWhenMagicRowsCols = Test
  { mark        = getMarks m
  , description = "Checking that your implementation of `isMagicSquare`\
                  \ returns `True` for squares where only rows and columns\
                  \ sum to the same value..."
  , successMsg  = "You got " ++ show m ++ " because your implementation of `isMagicSquare`\
                  \ returns `True` for squares where only rows and columns\
                  \ sum to the same value..."
  , failMsg     = "Your implementation did not return `True` for squares where\
                  \ only rows and columns sum to the same value"
  , prop        = makeUnaryPropWith
                    prop_semiMagicTrue
                    genNonMagicSquare
                    (const [])
  , condition   = IfFail test_trueWhenMagic
  }
  where 
    m = Marks 4

{- Question 5 -}

question5 = Question {name = "Question 5", tests = 
  [ test_circuitEasyNot
  , test_circuitEasyAnd
  , test_circuitEasyOr
  , test_circuitEasyImplies
  , test_circuitHardOnlyAnd
  , test_circuitHardOrFree
  , test_circuitHardImpliesFree
  ]
}

test_circuitEasyNot :: Test
test_circuitEasyNot = Test
  { mark        = getMarks m
  , description = newSection
                  ++
                  unwords
                    [ "Checking that your 'circuit' function"
                    , "works correctly on simple"
                    , "expressions of the form"
                    , "'Not (Var x)'..." ]
  , successMsg  = unwords
                    [ "You got"
                    , show m
                    , "because your 'circuit' function"
                    , "worked correctly"
                    , "on simple expressions of the form"
                    , singleQuotes "Not (Var x)" ++ "."
                    ]
  , failMsg     = unwords
                    [ "Your 'circuit' function"
                    , "did not work correctly on simple expressions of the form"
                    , singleQuotes "Not (Var x)" ++ "."
                    ]
  , prop        = makeUnaryPropWith
                    makeCircuitProperty
                    (arbitraryVar >>= return . Not)
                    shrink
  , condition   = Always
  }
  where
    m = Marks 2

makeBinConnectiveEasyCircuitTest :: BinConnective -> Test
makeBinConnectiveEasyCircuitTest conn = Test
  { mark        = getMarks m
  , description = unwords
                    [ "Checking that your 'circuit' function"
                    , "works correctly on simple"
                    , "expressions of the form"
                    , singleQuotes (showConn conn ++ " (Var x) (Var y)") ++ "..."
                    ]
  , successMsg  = unwords
                    [ "You got"
                    , show m
                    , "because your 'circuit' function"
                    , "worked correctly"
                    , "on simple expressions of the form"
                    , singleQuotes (showConn conn ++ " (Var x) (Var y)") ++ "."
                    ]
  , failMsg     = unwords
                    [ "Your 'circuit' function"
                    , "did not work correctly on simple expressions of the form"
                    , singleQuotes (showConn conn ++ " (Var x) (Var y)") ++ "."
                    ]
  , prop        = makeUnaryPropWith
                   makeCircuitProperty
                   (arbitraryVarPair <&> uncurry (binConnective conn))
                   shrink
  , condition   = Always
  }
  where
    m = Marks 2

test_circuitEasyAnd, test_circuitEasyOr, test_circuitEasyImplies :: Test

test_circuitEasyAnd     = makeBinConnectiveEasyCircuitTest Conj
test_circuitEasyOr      = makeBinConnectiveEasyCircuitTest Disj
test_circuitEasyImplies = makeBinConnectiveEasyCircuitTest Impl

test_circuitHardOnlyNot :: Test
test_circuitHardOnlyNot = Test
  { mark        = getMarks m
  , description = unwords
                    [ "Checking that your 'circuit' function"
                    , "works correctly on expressions containing only 'Not'"
                    , "as a connective..."
                    ]
  , successMsg  = unwords
                    [ "You got"
                    , show m
                    , "because your 'circuit' function"
                    , "worked correctly on expressions containing only 'Not'"
                    , "as a connective."
                    ]
  , failMsg    = unwords
                   [ "Your 'circuit' function did not work correctly"
                   , "on expressions containing only 'Not' as a connective."
                   ]
  , prop       = makeUnaryPropWith
                   makeCircuitProperty
                   arbitraryExprOnlyNot
                   shrink
  , condition  = Always
  }
  where
    m = Marks 4


test_circuitHardOnlyAnd :: Test
test_circuitHardOnlyAnd = Test
  { mark        = getMarks m
  , description = unwords
                  [ "Checking that your 'circuit' function"
                  , "works correctly on expressions containing only 'And'"
                  , "as a connective..."
                  ]
  , successMsg = unwords
                   [ "You got"
                   , show m
                   , "because your 'circuit' function"
                   , "worked correctly on expressions containing only 'And'"
                   , "as a connective."
                   ]
  , failMsg    = unwords
                   [ "Your 'circuit' function did not work correctly"
                   , "on expressions containing only 'And' as a connective."
                   ]
  , prop       = makeUnaryPropWith
                   makeCircuitProperty
                   arbitraryExprOnlyAnd
                   shrink
  , condition  = Always
  }
  where
    m = Marks 4

makeCircuitBinConnectiveHardTest :: BinConnective -> Test
makeCircuitBinConnectiveHardTest conn = Test
  { mark        = getMarks m
  , description = unwords
                    [ "Checking that your 'circuit' function"
                    , "works correctly on expressions free of"
                    , vernConn conn ++ "s..."
                    ]
  , successMsg  = unwords
                    [ "You got"
                    , show m
                    , "because your 'circuit' function"
                    , "worked correctly on expressions free of"
                    , vernConn conn ++ "s."
                    ]
  , failMsg     = unwords
                    [ "Your 'circuit' function"
                    , "did not work correctly on expressions free of"
                    , vernConn conn ++ "s."
                    ]
  , prop        = makeUnaryPropWith
                    makeCircuitProperty
                    (arbitraryExprFreeOf conn)
                    shrink
  , condition   = Always
  }
  where
    m = Marks 4

test_circuitHardOrFree      :: Test
test_circuitHardImpliesFree :: Test

test_circuitHardOrFree      = makeCircuitBinConnectiveHardTest Disj
test_circuitHardImpliesFree = makeCircuitBinConnectiveHardTest Impl