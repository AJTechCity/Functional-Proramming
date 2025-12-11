#!/bin/sh

if [ "$1" = "" ]
then
    echo "You forgot to add the assignment name: 'MockTest'."
    echo "Please run the script again with the right argument."
    exit 1
fi

if ! [ -f "$1.hs" ]
then
    echo "File '$1.hs' not found."
    echo "Are you in the correct directory?"
    exit 1
fi

echo "Checking if you are runnning on vLab..."

if ! uname -a | grep "vlab";
then
    echo "You are running the script in the environment different from vLab."
    echo "Please run the ./presubmit.sh on vLab."
    exit 1
fi

echo "Trying to compile your submission..."

# Create temporary directory
temp_dir=$(mktemp -d)

ghc $1.hs -odir $temp_dir -hidir $temp_dir

if [ $? -ne 0 ]
then
    echo ""
    echo "Your file '$1.hs' did not compile."
    echo "Please fix it before submitting."
    exit 1
fi

if ! [ -f "$temp_dir/$1.o" ]
then
    echo ""
    echo "The module name in '$1.hs' does match not the filename '$1'."
    echo "Please make sure you that"
    echo -e "\t(i) your file is called 'MockTest.hs'"
    echo -e "\t(ii) you did not change the top of the template"
    echo "and try again."
    exit 1
fi

ghc -XSafe -XNoGeneralizedNewtypeDeriving $1.hs -odir $temp_dir -hidir $temp_dir

if [ $? -ne 0 ]
then
    echo ""
    echo "Your file did not compile with '-XSafe.'"
    echo "Did you remove '{-# LANGUAGE Safe #-}' from the template?"
    exit 1
fi

# Create file for ensuring type signatures have not been modified

cat >> $temp_dir/Signatures.hs << 'END'
-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns -Wno-x-partial #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE NoGeneralizedNewtypeDeriving, Safe #-}

module Signatures where

import Types
import MockTest

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
-- QUESTION 1
---------------------------------------------------------------------------------

isNBranching :: Int -> Rose a -> Bool
isNBranching = MockTest.isNBranching

prune :: Int -> Rose a -> Rose a
prune = MockTest.prune 

---------------------------------------------------------------------------------
-- QUESTION 2
---------------------------------------------------------------------------------

applyNTimes :: Monad m => m a -> (a -> m a) -> Int -> m [a]
applyNTimes = MockTest.applyNTimes

---------------------------------------------------------------------------------
-- QUESTION 3
---------------------------------------------------------------------------------

gameOver :: NimGame Bool
gameOver = MockTest.gameOver 

takeTokens :: Int -> Heap -> NimGame ()
takeTokens = MockTest.takeTokens

---------------------------------------------------------------------------------
-- QUESTION 4
---------------------------------------------------------------------------------

isMagicSquare :: [[Int]] -> Bool
isMagicSquare = MockTest.isMagicSquare

---------------------------------------------------------------------------------
-- QUESTION 5
---------------------------------------------------------------------------------

circuit :: Expr -> Circuit
circuit = MockTest.circuit

END

ghc -XSafe -XNoGeneralizedNewtypeDeriving $temp_dir/Signatures.hs -odir $temp_dir -hidir $temp_dir

if [ $? -ne 0 ]
then
    echo ""
    echo "Your file did not compile with the correct type signatures."
    echo "Did you modify the type signatures or header from the template?"
    exit 1
fi


# Checking if they modified the header

echo "Checking if the header is unchanged..."

cat >> $temp_dir/Signatures2.hs << 'END'

-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns -Wno-x-partial #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE NoGeneralizedNewtypeDeriving, Safe #-}

module MockTest ( isNBranching
                 , prune
                 , applyNTimes
                 , gameOver
                 , takeTokens
                 , isMagicSquare
                 , circuit
                 ) where

import Types
import Data.List

newtype Q7fk32Lp a = Q7fk32Lp a
END

sed -E '1,/DO \*\*NOT\*\* MAKE ANY CHANGES ABOVE THIS LINE/d' $1.hs >> $temp_dir/Signatures2.hs


ghc -XSafe -XNoGeneralizedNewtypeDeriving $temp_dir/Signatures2.hs -odir $temp_dir -hidir $temp_dir

if [ $? -ne 0 ]
then
    echo ""
    echo "Your file did not compile with the original header."
    echo "Did you add extra imports or modify the header?"
    echo "Make sure you do not remove the block"
    echo "---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------"
    echo "and anything above it"
    exit 1
fi




echo ""
echo "All checks passed."
echo "You are ready to submit!"

# Cleanup temporary directory
rm -r $temp_dir
