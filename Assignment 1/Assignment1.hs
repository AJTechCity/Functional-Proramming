-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE NoGeneralizedNewtypeDeriving, Safe #-}

module Assignment1 ( put,
                     moveLeft,
                     moveRight,
                     moveUp,
                     moveDown,
                     Grid(..),
                     GridWithAPointer(..),
                     putTatamiDown,
                     putTatamiUp,
                     putTatamiLeft,
                     putTatamiRight,
                     cover
             ) where

import Data.Char (isLetter)

-- these two function are to correctly measure the width of an entry of a grid, 
-- i.e. so that the width of "\ESC[44m55\ESC[0m" ignored the escape sequences
stripANSI :: String -> String
stripANSI [] = []
stripANSI ('\ESC':'[':xs) = stripANSI (drop 1 (dropWhile (not . isLetter) xs))
stripANSI (x:xs) = x : stripANSI xs

visibleLength :: String -> Int
visibleLength = length . stripANSI

newtype Grid a = Grid { grid :: [[a]] } deriving Eq

instance (Show a) => Show (Grid a) where
  show (Grid g)
    | null g = ""
    | otherwise = unlines (map showRow g)
    where
      strGrid = map (map show) g
      colWidths = [maximum (map visibleLength col) | col <- transpose strGrid]
      showRow row = unwords [padRight w s | (w, s) <- zip colWidths (map show row)]
      padRight n s = s ++ replicate (n - visibleLength s) ' '

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)


newtype GridWithAPointer a = GridWithAPointer (Grid a, [a], a, [a], Grid a)
  deriving Eq


---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
-- TASK 1
---------------------------------------------------------------------------------

blueHighlight :: String -> String
blueHighlight s = "\ESC[44m" ++ s ++ "\ESC[0m"

instance (Show a) => Show (GridWithAPointer a) where
     show (GridWithAPointer (Grid topGrid, rowLeft, pointer, rowRight, Grid bottomGrid)) = 
      unlines (map unwords fullGrid)
      where
        highlight = blueHighlight (show pointer) --String
        middle = reverse (map show rowLeft) ++ [highlight] ++ map show rowRight -- Want [String]
        fullGrid = map (map show) topGrid ++ [middle] ++ map (map show) bottomGrid -- [[String]]

---------------------------------------------------------------------------------
-- TASK 2
---------------------------------------------------------------------------------

put :: a -> GridWithAPointer a -> GridWithAPointer a
put x (GridWithAPointer(topGrid, left, _, right, bottomGrid)) = 
  GridWithAPointer(topGrid, left, x, right, bottomGrid)

moveLeft :: GridWithAPointer a -> GridWithAPointer a
moveLeft (GridWithAPointer(topGrid, left, pointer, right, bottomGrid)) = 
  GridWithAPointer(topGrid, tail(left), head(left), [pointer] ++ right, bottomGrid)


moveRight :: GridWithAPointer a -> GridWithAPointer a
moveRight (GridWithAPointer(topGrid, left, pointer, right, bottomGrid)) = 
  GridWithAPointer(topGrid, [pointer] ++ left, head(right), tail(right), bottomGrid)

moveUp :: GridWithAPointer a -> GridWithAPointer a
moveUp (GridWithAPointer(topGrid, left, pointer, right, bottomGrid))= 
  let
    topRows = grid topGrid
    bottomRows = grid bottomGrid
    newMidRow = last(topRows)
  in
    GridWithAPointer(Grid(reverse(tail(reverse topRows))), reverse(take (length left) newMidRow), newMidRow !! (length left) , drop (length left +1) newMidRow , Grid([reverse left ++ [pointer] ++ right] ++ bottomRows))

moveDown :: GridWithAPointer a -> GridWithAPointer a
moveDown (GridWithAPointer(topGrid, left, pointer, right, bottomGrid)) = 
  let
    topRows = grid topGrid
    bottomRows = grid bottomGrid
    newMidRow = head(bottomRows)
  in
    GridWithAPointer(Grid(topRows ++ [reverse(left) ++ [pointer] ++ right]), reverse(take (length left) newMidRow), newMidRow !! (length left), drop (length left +1) newMidRow, Grid(tail(bottomRows)))


---------------------------------------------------------------------------------
-- TASK 3
---------------------------------------------------------------------------------

putTatamiUp :: Integer -> GridWithAPointer Integer -> GridWithAPointer Integer
putTatamiUp num gwp = moveDown (put num (moveUp (put num gwp)))

putTatamiDown :: Integer -> GridWithAPointer Integer -> GridWithAPointer Integer
putTatamiDown num gwp = moveUp (put num (moveDown (put num gwp)))

putTatamiRight :: Integer -> GridWithAPointer Integer -> GridWithAPointer Integer
putTatamiRight num gwp = moveLeft (put num (moveRight(put num gwp)))

putTatamiLeft :: Integer -> GridWithAPointer Integer -> GridWithAPointer Integer
putTatamiLeft num gwp = moveRight (put num (moveLeft(put num gwp)))


---------------------------------------------------------------------------------
-- TASK 4
---------------------------------------------------------------------------------

canPlaceTatamiRight :: GridWithAPointer Integer -> Bool
canPlaceTatamiRight (GridWithAPointer (_ ,_ ,current ,(r:_) ,_)) =
  current == 0 && r == 0
canPlaceTatamiRight _ = False

canPlaceTatamiLeft :: GridWithAPointer Integer -> Bool
canPlaceTatamiLeft (GridWithAPointer (_, (l:_) ,current ,_ ,_)) =
    current == 0 && l == 0
canPlaceTatamiLeft _ = False

canPlaceTatamiDown :: GridWithAPointer Integer -> Bool
canPlaceTatamiDown (GridWithAPointer (_, left, current, _, Grid belowRows)) =
  let colIndex = length left
  in case belowRows of
       (rowBelow : _) ->
         current == 0 &&
         colIndex < length rowBelow &&
         rowBelow !! colIndex == 0
       [] -> False

canPlaceTatamiUp :: GridWithAPointer Integer -> Bool
canPlaceTatamiUp (GridWithAPointer (Grid aboveRows, left, current, _, _)) =
  let colIndex = length left
  in case reverse aboveRows of
       (rowAbove : _) ->
         current == 0 &&
         colIndex < length rowAbove &&
         rowAbove !! colIndex == 0
       [] -> False

count :: [Integer] -> Integer
count [] = 0
count (x:xs)
    | x > 0     = 1 + count xs
    | otherwise = count xs

cost :: GridWithAPointer Integer -> Integer
cost (GridWithAPointer (above, left, current, right, below)) =
    let leftCount = count left
        rightCount = count right
        currentCount = count [current]
        aboveCount   = count (concat (grid above))
        belowCount   = count (concat (grid below))
        total = leftCount + rightCount + currentCount + aboveCount + belowCount
    in total  
    
flatten :: GridWithAPointer Integer -> Grid Integer
flatten (GridWithAPointer (Grid above, left, current, right, Grid below)) =
  let middle = reverse left ++ [current] ++ right
  in Grid (above ++ [middle] ++ below)
    
nub :: Eq a => [a] -> [a]
nub []     = []
nub (x:xs) = x : nub (filter (/= x) xs)

noClash :: GridWithAPointer Integer -> Bool
noClash gwp =
  let Grid rows = flatten gwp
      numRows = length rows
      numCols = length (head rows)
      isCornerClash r c =
        let ids = [ rows !! r     !! c
                  , rows !! r     !! (c+1)
                  , rows !! (r+1) !! c
                  , rows !! (r+1) !! (c+1)
                  ]
        in length (nub ids) == 4 && all (/= 0) ids
  in and [ not (isCornerClash r c) | r <- [0 .. numRows - 2], c <- [0 .. numCols - 2] ]

advancePointerOnce :: GridWithAPointer Integer -> Maybe (GridWithAPointer Integer)
advancePointerOnce g@(GridWithAPointer (Grid aboveRows, left, current, right, Grid belowRows)) =
  case right of
    (_:_) -> Just (moveRight g)                        
    [] ->
      case belowRows of
        (rowBelow : rowsBelow) ->                       
          let newG = moveDownToColumnZero g
          in Just newG
        [] -> Nothing

moveDownToColumnZero :: GridWithAPointer Integer -> GridWithAPointer Integer
moveDownToColumnZero g =
  let g' = moveDown g
      goLeft gg@(GridWithAPointer (_, l, _, _, _)) =
        case l of
          [] -> gg
          _  -> goLeft (moveLeft gg)
  in goLeft g'

findNextEmpty :: GridWithAPointer Integer -> Maybe (GridWithAPointer Integer)
findNextEmpty start = loop start
  where
    loop g@(GridWithAPointer (_, _, current, _, _))
      | current == 0 = Just g
      | otherwise =
          case advancePointerOnce g of
            Just g' -> loop g'
            Nothing -> Nothing


pointerCoords :: GridWithAPointer a -> (Int, Int)
pointerCoords (GridWithAPointer (Grid aboveRows, left, _, _, _)) = (length aboveRows, length left)

moveTo :: (Int, Int) -> GridWithAPointer Integer -> GridWithAPointer Integer
moveTo (tr, tc) g0 = goRow curR (goCol curC g0)
  where
    (curR, curC) = pointerCoords g0
    goCol c g
      | c < tc   = goCol (c + 1) (moveRight g)
      | c > tc   = goCol (c - 1) (moveLeft g)
      | otherwise = g
    goRow r g
      | r < tr   = goRow (r + 1) (moveDown g)
      | r > tr   = goRow (r - 1) (moveUp g)
      | otherwise = g

rowsOf :: GridWithAPointer Integer -> [[Integer]]
rowsOf gwp = let Grid rs = flatten gwp in rs

elemIndex :: Eq a => a -> [a] -> Maybe Int
elemIndex _ [] = Nothing
elemIndex x (y:ys)
  | x == y    = Just 0
  | otherwise = fmap (+1) (elemIndex x ys)

findFirstEmptyCoord :: GridWithAPointer Integer -> Maybe (Int, Int)
findFirstEmptyCoord g =
  let rs = rowsOf g
      findInRow r row = case elemIndex 0 row of
                          Just c  -> Just (r, c)
                          Nothing -> Nothing
      search [] _ = Nothing
      search (r:rs') idx =
        case findInRow idx r of
          Just p  -> Just p
          Nothing -> search rs' (idx + 1)
  in search rs 0

candidatesAt :: Integer -> GridWithAPointer Integer -> [GridWithAPointer Integer]
candidatesAt n g =
  catMaybes
    [ if canPlaceTatamiUp g
        then let g' = putTatamiUp n g in if noClash g' then Just g' else Nothing
        else Nothing
    , if canPlaceTatamiDown g
        then let g' = putTatamiDown n g in if noClash g' then Just g' else Nothing
        else Nothing
    , if canPlaceTatamiLeft g
        then let g' = putTatamiLeft n g in if noClash g' then Just g' else Nothing
        else Nothing
    , if canPlaceTatamiRight g
        then let g' = putTatamiRight n g in if noClash g' then Just g' else Nothing
        else Nothing
    ]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes (Just y:xs)  = y : catMaybes xs

isFull :: GridWithAPointer Integer -> Bool
isFull g = all (/= 0) (concat (rowsOf g))

searchCover :: Integer -> GridWithAPointer Integer -> Maybe (GridWithAPointer Integer)
searchCover n g
  | isFull g  = Just g
  | otherwise =
      case findFirstEmptyCoord g of
        Nothing -> if isFull g then Just g else Nothing
        Just coord ->
          let gAt = moveTo coord g
              cand = candidatesAt n gAt
              tryCandidates []     = Nothing
              tryCandidates (x:xs) =
                case searchCover (n + 1) x of
                  Just sol -> Just sol
                  Nothing  -> tryCandidates xs
          in tryCandidates cand

cover :: GridWithAPointer Integer -> GridWithAPointer Integer
cover g =
  case searchCover 1 g of
    Just sol -> sol
    Nothing  -> error "cover: no complete tatami covering exists for this grid"