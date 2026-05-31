import Data.Char

--Text messaging

-----------------------------------------
-- | 1      | 2 ABC | 3 DEF  |
-----------------------------------------
-- | 4 GHI  | 5 JKL | 6 MNO  |
-----------------------------------------
-- | 7 PQRS | 8 TUV | 9 WXYZ |
-----------------------------------------
-- | *      | 0     | # .,   |
-----------------------------------------


-- Valid buttons are ['0'..'9']++['*','#']
type Button = Char
-- Valid presses are [1..]
type Presses = Int
-- Valid text consists of
-- ['A'..'Z']++['a'...'z']++['0'..'9']++['.',',',' ']
type Text = String

--Exercise 1a - takes a list of buttons and the number of times to press them and gives back the corresponding text
buttonChars :: Button -> [Char]
buttonChars '2' = ['a', 'b', 'c', '2']
buttonChars '3' = ['d', 'e', 'f', '3']
buttonChars '4' = ['g', 'h', 'i', '4']
buttonChars '5' = ['j', 'k', 'l', '5']
buttonChars '6' = ['m', 'n' , 'o', '6']
buttonChars '7' = ['p', 'q', 'r', 's', '7']
buttonChars '8' = ['t', 'u', 'v', '8']
buttonChars '9' = ['w', 'x', 'y', 'z', '9']
buttonChars '0' = [' ', '0']
buttonChars '1' = ['1']
buttonChars '#' = ['.', ',']
buttonChars _ = []

pressesToChar :: (Button, Presses) -> Char
pressesToChar (btn, 0) = btn
pressesToChar (btn, presses) = 
    let charList = buttonChars btn
        charLen = length charList
        index = if charLen == 0 then 0 else (presses-1) `mod` charLen
        selected = charList !! index
    in
        selected

phoneToString :: [(Button, Presses)] -> Text
phoneToString [] = ""
phoneToString [x] = [pressesToChar x]
phoneToString ((btn, presses):y:xs) = 
    let isCap = if btn=='*' then True else False
        char = if isCap then toUpper (pressesToChar y) else pressesToChar (btn, presses)
    in
        [pressesToChar (btn, presses)] ++ (if isCap then phoneToString xs else phoneToString ([y]++xs))