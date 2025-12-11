-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE NoGeneralizedNewtypeDeriving, Safe #-}

---------------------------------------------------------------------------------
-------------------------- DO **NOT** MODIFY THIS FILE --------------------------
---------------------------------------------------------------------------------

module Types (module Types, module Control.Monad, module Control.Monad.State) where

import Control.Monad
import Control.Monad.State

---------------------------------------------------------------------------------
-- QUESTION 1
---------------------------------------------------------------------------------

data Rose a = Leaf a
            | Branch [ Rose a ]
            deriving Show

---------------------------------------------------------------------------------
-- QUESTION 2
---------------------------------------------------------------------------------

addAndPrint :: Int -> IO Int
addAndPrint n = do putStrLn $ show n
                   return $ n + 2

---------------------------------------------------------------------------------
-- QUESTION 3
---------------------------------------------------------------------------------

type NimBoard = (Int,Int)
type NimGame a = State NimBoard a 

data Heap = First | Second

---------------------------------------------------------------------------------
-- QUESTION 4
---------------------------------------------------------------------------------

magicSquare1 :: [[Int]]
magicSquare1 = [[8,1,6],[3,5,7],[4,9,2]]

magicSquare2 :: [[Int]]
magicSquare2 = [[1,2,3],[4,5,6],[7,8,9]]

---------------------------------------------------------------------------------
-- QUESTION 5
---------------------------------------------------------------------------------

data Expr = Var     Char
          | Not     Expr
          | And     Expr Expr
          | Or      Expr Expr
          | Implies Expr Expr
          deriving (Eq, Show)

data Circuit = Input Char
             | Nand  Circuit Circuit
             deriving (Eq, Show)

expr1 :: Expr
expr1 = Not (Var 'p')

expr2 :: Expr
expr2 = And (Var 'p') (Var 'q')

expr3 :: Expr
expr3 = Or (Not (Var 'p')) (Var 'q')
