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
buttons :: [(Button, String)]
buttons =
    [ ('1',"1")
    , ('2',"abc2")
    , ('3',"def3")
    , ('4',"ghi4")
    , ('5',"jkl5")
    , ('6',"mno6")
    , ('7',"pqrs7")
    , ('8',"tuv8")
    , ('9',"wxyz9")
    , ('0'," 0")
    , ('#',".,")
    ]

buttonChars :: Button -> String
buttonChars b = 
    case lookup b buttons of
        Just s -> s
        Nothing -> ""

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

--Exercise 1b - taking a string to a list of buttons and the number of times that they need to be pressed

charToPresses :: Char -> [(Button, Presses)]
charToPresses x = undefined

stringToPhone :: Text -> [(Button, Presses)]
stringToPhone = undefined


--Using Maybe Types
--Exercise 1 - Rewrite head and tail so they use Maybe
headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:xs) = Just x

tailMaybe :: [a] -> Maybe [a]
tailMaybe [] = Nothing
tailMaybe (x:xs) = Just xs

--Exercise 2 - Rewrite take to use Maybe to indicate when index is longer than list
takeMaybe :: Int -> [a] -> Maybe [a]
takeMaybe 0 _ = Just []
takeMaybe _ [] = Nothing
takeMaybe num (x:xs) = case takeMaybe (num-1) xs of
    Nothing -> Nothing
    Just ys -> Just (x:ys)

--Exercise 3 - Rewrite zip from prelude using Either - If 2 arguments are same length, put them in tuple, otherwise return the shortest one as String
zipEither :: [a] -> [b] -> Either String [(a,b)]
zipEither [] [] = Right []
zipEither [] _ = Left "1st Argument Smaller"
zipEither _ [] = Left "2nd Argument Smaller"
zipEither (x:xs) (y:ys) = case zipEither xs ys of
    Left err -> Left err
    Right pairs -> Right ((x,y) : pairs)

--Type Retractions
--Exercise 1 - 
data WeekDay = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show, Read, Eq, Ord, Enum)
data WorkingDay = MonW | TueW | WedW | ThuW | FriW deriving (Show, Eq)

toWeekDay :: WorkingDay -> WeekDay
toWeekDay MonW = Mon
toWeekDay TueW = Tue
toWeekDay WedW = Wed
toWeekDay ThuW = Thu
toWeekDay FriW = Fri

toWorkingDay :: WeekDay -> WorkingDay
toWorkingDay Mon = MonW
toWorkingDay Tue = TueW
toWorkingDay Wed = WedW
toWorkingDay Thu = ThuW
toWorkingDay Fri = FriW
toWorkingDay Sat = error "No Working Day for Sat"
toWorkingDay Sun = error "No Working Day for Sun"

--Exercise 2 - Show type Maybe a is retract of the type [a]
toList :: Maybe a -> [a]
toList (Just a) = [a]
toList (Nothing) = []

toMaybe :: [a] -> Maybe a
toMaybe [] = Nothing
toMaybe (x:xs) = Just x