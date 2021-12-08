module Model.Computer where

import Model.Board
import System.Random -- (Random(randomRIO))
import System.IO.Unsafe (unsafePerformIO)

-------------------------------------------------------------------------------
-- | Helper functions taken by opponent player (computer) ---------------------
-------------------------------------------------------------------------------
generateCode :: Code
generateCode = unsafePerformIO (generateCodeHelper colors [] cols)
  where
    colors = allColors

generateCodeHelper :: [Color] -> [Color] -> Int -> IO [Color]
generateCodeHelper _ currColors 0 = return currColors
generateCodeHelper allColors currColors numColors = 
  do
    newCurrColors <- generateColorNoDupes allColors currColors
    generateCodeHelper allColors newCurrColors (numColors - 1)


generateColorNoDupes :: [Color] -> [Color] -> IO [Color]
generateColorNoDupes allColors currColors = 
  do
    colorIndex <- randomRIO (0, length allColors - 1)
    let color = allColors !! colorIndex
    if elem color currColors
      then generateColorNoDupes allColors currColors
      else return (color : currColors)

-------------------------------------------------------------------------------
-- | Logic functions ----------------------------------------------------------
-------------------------------------------------------------------------------
generateHints :: Code -> Board -> Int -> Hints -> Hints
generateHints code b row currHints = insertHintRow currHints hintList row
  where
    hintList    = [getHintByPos hintCounts row col | col <- [1..cols]]
    hintCounts  = countHintTypes code colors
    colors      = map (\maybeColor -> maybeColorToColor maybeColor) maybeColors
    maybeColors = [boardLookup b (Pos row col) | col <- [1..cols]]

-- Inserts hints based off (ColorPos, Color, Incorrect) tuple
getHintByPos :: (Int, Int, Int) -> Int -> Int -> Hint
getHintByPos (cp,c,i) row col 
  | cp >= col       = ColorPos
  | (cp + c) >= col = Color
  | True            = Incorrect

countHintTypes :: Code -> [Color] -> (Int, Int, Int)
countHintTypes code colors = (colorPosCount, colorCount, incorrectCount)
  where
    incorrectCount = cols - colorCount - colorPosCount
    (colorCount,_) = countColor codeColors' colors
    (colorPosCount,codeColors') = countColorPos codeColors colors
    codeColors = map (\col -> code !! col) [0..cols-1]


countColor :: [Color] -> [Color] -> (Int, [Color])
countColor code colors = countColorHelper (0,code) colors

countColorHelper :: (Int, [Color]) -> [Color] -> (Int, [Color])
countColorHelper carry [] = carry
countColorHelper (count,code) (color:colors) = countColorHelper (count', code') colors
  where
    (count', code') = if elem color code
                               then (count+1, deleteElem color code)
                               else (count, code)

countColorPos :: [Color] -> [Color] -> (Int, [Color])
countColorPos code colors = countColorPosHelper (0,[]) code colors

countColorPosHelper :: (Int, [Color]) -> [Color] -> [Color] -> (Int, [Color])
countColorPosHelper carry _ [] = carry 
countColorPosHelper (count, carryColors) (code:codes) (color:colors) = countColorPosHelper (count', carryColors') codes colors
  where
    (count', carryColors') = if code == color
                               then (count+1, carryColors)
                               else (count, code:carryColors)


deleteElem :: Eq a => a -> [a] -> [a]
deleteElem element [] = []
deleteElem element (a:as) = if a == element
                            then as
                            else [a] ++ deleteElem element as

-- >>> [generateHint [Orange,Yellow,Blue,Pink] (M.insert (Pos 1 1) Yellow (M.insert (Pos 1 2) Orange (M.insert (Pos 1 3) Blue (M.insert (Pos 1 4) Pink M.empty)))) 1 col | col <- [1..cols]]
-- [Color,Color,ColorPos,ColorPos]
--

-- >>> generateHints [Orange,Yellow,Blue,Pink] (M.insert (Pos 1 1) Yellow (M.insert (Pos 1 2) Orange (M.insert (Pos 1 3) Blue (M.insert (Pos 1 4) Pink M.empty)))) 1 M.empty
-- fromList [(Pos {pRow = 1, pCol = 1},ColorPos),(Pos {pRow = 1, pCol = 2},ColorPos),(Pos {pRow = 1, pCol = 3},Color),(Pos {pRow = 1, pCol = 4},Color)]
--

-- >>> compareWithCode [Blue,Green,Red,Orange] Blue 1
-- ColorPos
--


interpretResult :: Code -> Board -> Int -> Maybe Result
interpretResult c b turn = 
  if correctness
    then Just Win
    else if isLastTurn
      then Just Lose
      else Nothing
  where
    correctness = isRowCorrect c b turn 1
    isLastTurn  = turn == rows

isRowCorrect :: Code -> Board -> Int -> Int -> Bool
isRowCorrect _ _ _   5 = True
isRowCorrect code b row col = 
  if correct
    then isRowCorrect code b row (col+1)
    else False
  where
    correct    = case maybeColor of
                  Nothing -> False
                  Just color -> color == codeColor
    maybeColor = boardLookup b (Pos row col)
    codeColor  = code !! (col-1)