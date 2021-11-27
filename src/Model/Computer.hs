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
isRowCorrect c b row (5) = True
isRowCorrect c b row col = if isCellCorrect c b row col
                             then isRowCorrect c b row (col+1)
                             else False

isCellCorrect :: Code -> Board -> Int -> Int -> Bool
isCellCorrect c b row col = 
  case actualColor of
    Nothing -> False
    Just c  -> c == expectedColor
  where
    actualColor   = (boardLookup b (Pos row col))
    expectedColor = (c !! col)