-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE NoGeneralizedNewtypeDeriving, Safe #-}

module Assignment2 (encodeWord , encodeWords , encodeText ,
                    decodeText ,
                    decodeTextWithTree ,
                    ramify ,
                    tabulate ,
                    tree) where

import Types
import Data.List

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

{- Question 1 -}
encodeWord :: Table -> String -> Code
encodeWord table "" = []
encodeWord table [x] = 
    case lookup x table of
        Just code -> code
        Nothing -> []
encodeWord table (x:xs) =
    case lookup x table of
        Just code -> code ++ shortGap ++ encodeWord table xs
        Nothing -> encodeWord table xs

encodeWords :: Table -> [String] -> Code
encodeWords table [] = []
encodeWords table [x] = encodeWord table x
encodeWords table (x:xs) = encodeWord table x ++ mediumGap ++ encodeWords table xs

encodeText :: Table -> String -> Code
encodeText table inputString = encodeWords table (words inputString)

{- Question 2 -}
reverseTable :: Table -> [(Code, Char)]
reverseTable table = map swap table
    where
        swap (a, b) = (b, a)

takeLetter :: Code -> (Code, Code)
takeLetter [] = ([], [])
takeLetter code
    | dit `isPrefixOf` code = 
        let (restLetter, restCode) = takeLetter (drop (length dit) code)
        in (dit ++ restLetter, restCode)
    | dah `isPrefixOf` code=
        let (restLetter, restCode) = takeLetter (drop (length dah) code)
        in (dah ++ restLetter, restCode)
    | mediumGap `isPrefixOf` code = 
        ([], code)
    | shortGap `isPrefixOf` code = 
        ([], drop (length shortGap) code)
    | otherwise = ([], code)

decodeCharCode :: Table -> Code -> String --DONE - Works
decodeCharCode table charCode = 
  case lookup charCode reversedTable of
    Just char -> [char]
    Nothing -> ['?']
  where
    reversedTable = reverseTable table

decodeText :: Table -> Code -> String
decodeText table [] = ""
decodeText table code
    | (mediumGap `isPrefixOf` code) = " " ++ decodeText table (drop (length mediumGap) code)
    | otherwise = 
        decodeCharCode table (charCode) ++ decodeText table restCode
        where
            (charCode, restCode) = takeLetter code

{- Question 3 -}
findTreeDirection :: Code -> Maybe (Bool, Code)
findTreeDirection code
    | dit `isPrefixOf` code = Just (False, drop (length dit) code)
    | dah `isPrefixOf` code = Just (True, drop (length dah) code)
    | otherwise = Nothing

walkTreeToChar :: Tree -> Code -> Char
walkTreeToChar Empty _ = '?'
walkTreeToChar (Branch maybeChar left right) [] = 
    case maybeChar of
        Just c -> c
        Nothing -> '?' -- Invalid Code supplied
walkTreeToChar (Branch maybeChar left right) path = 
    case findTreeDirection path of
        Just (dir, newPath) ->
            walkTreeToChar (if dir then right else left) newPath
        Nothing -> '?' -- Invalid Code


decodeTextWithTree :: Tree -> Code-> String
--Assume left branch is dit and right branch is dah
decodeTextWithTree _ [] = ""
decodeTextWithTree tree code
    | (mediumGap `isPrefixOf` code) = " " ++ decodeTextWithTree tree (drop (length mediumGap) code)
    | otherwise = 
        [walkTreeToChar tree charPath] ++ (decodeTextWithTree tree restCode)
        where
            (charPath, restCode) = takeLetter code


{- Question 4 -}
--Convert Table into Tree
breakdownPath :: Code -> Maybe (Bool, Code)
breakdownPath code --Returns False if it should be left sub-tree (dit) or True if it should be right sub-tree (dah)
    | dit `isPrefixOf` code = Just(False, drop (length dit) code)
    | dah `isPrefixOf` code = Just(True, drop (length dah) code)
    | otherwise = Nothing

updateNode :: Tree -> Char -> Code -> Tree
updateNode Empty char path = updateNode (Branch Nothing Empty Empty) char path -- base case to create new tree where required
updateNode (Branch _ left right) char [] = Branch (Just char) left right --Return the new updated node whilst keeping existing sub-trees
updateNode (Branch maybeChar left right) char path = 
    case breakdownPath path of
        Just (False, newPath) ->
            Branch maybeChar (updateNode left char newPath) right
        Just (True, newPath) ->
            Branch maybeChar left (updateNode right char newPath)
        Nothing -> (Branch maybeChar left right)

doRamify :: Table -> Tree -> Tree
doRamify [] tree = tree --Empty base-case
doRamify ((char, code):xs) tree = 
    doRamify xs (updateNode tree char code)

    
--Perform DFS or BFS and store char's in array alongside pathway in another array, then zip together the arrays
ramify :: Table -> Tree
ramify table = 
    doRamify table Empty

{- Question 5 -}
dfs :: Code -> Tree -> Table
dfs _ Empty = []
dfs currentPath (Branch maybeChar left right) = 
    newNode ++ (dfs (currentPath ++ dit) left) ++ (dfs (currentPath ++ dah) right)
    where
        newNode = case maybeChar of
            Just c -> [(c, currentPath)]
            Nothing -> []

tabulate :: Tree -> Table
tabulate tree = 
    dfs [] tree

{- Question 6 -}
brackets :: Bracket -> String
brackets (Round ts) = "(" ++ concat [brackets t | t <- ts] ++ ")"
brackets (Curly ts) = "{" ++ concat [brackets t | t <- ts] ++ "}"

tree :: String -> Maybe Bracket
tree = undefined

isWellBracketed :: String -> Bool
isWellBracketed xs = case tree xs of
    Nothing -> False
    Just _  -> True