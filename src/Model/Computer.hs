module Model.Computer where

import Model.Board
import System.Random

-------------------------------------------------------------------------------
-- | Helper functions taken by opponent player (computer) ---------------------
-------------------------------------------------------------------------------
generateCode :: Bool -> Int -> IO Code
generateCode allowDupes numColors = generateCodeHelper allowDupes colors [] numColors
  where
    colors = allColors

generateCodeHelper :: Bool -> [Color] -> [Color] -> Int -> IO [Color]
generateCodeHelper _          _ currColors 0 = return currColors
generateCodeHelper allowDupes allColors currColors numColors = 
  do
    newCurrColors <- generateColor allowDupes allColors currColors
    generateCodeHelper allowDupes allColors newCurrColors (numColors - 1)


generateColor :: Bool -> [Color] -> [Color] -> IO [Color]
generateColor allowDupes allColors currColors = 
  do
    colorIndex <- getStdRandom $ randomR (0, length allColors - 1)
    let color = allColors !! colorIndex
    if allowDupes == False && elem color currColors
      then generateColor allowDupes allColors currColors
      else return (color : currColors)

-------------------------------------------------------------------------------
-- | Logic functions ----------------------------------------------------------
-------------------------------------------------------------------------------
generateHints :: Int -> Code -> Board -> Int -> Hints -> Hints
generateHints cols code b row currHints = insertHintRow cols currHints hintList row
  where
    hintList    = [getHintByPos hintCounts row col | col <- [1..cols]]
    hintCounts  = countHintTypes cols code colors
    colors      = map (\maybeColor -> maybeColorToColor maybeColor) maybeColors
    maybeColors = [boardLookup b (Pos row col) | col <- [1..cols]]

-- Inserts hints based off (ColorPos, Color, Incorrect) tuple
getHintByPos :: (Int, Int, Int) -> Int -> Int -> Hint
getHintByPos (cp,c,i) row col 
  | cp >= col       = ColorPos
  | (cp + c) >= col = Color
  | True            = Incorrect

countHintTypes :: Int -> Code -> [Color] -> (Int, Int, Int)
countHintTypes cols code colors = (colorPosCount, colorCount, incorrectCount)
  where
    incorrectCount = cols - colorCount - colorPosCount
    (colorCount,_) = countColor codeColors' carryColors'
    (colorPosCount,codeColors',carryColors') = countColorPos codeColors colors
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

countColorPos :: [Color] -> [Color] -> (Int, [Color], [Color])
countColorPos code colors = countColorPosHelper (0,[],[]) code colors

countColorPosHelper :: (Int, [Color], [Color]) -> [Color] -> [Color] -> (Int, [Color], [Color])
countColorPosHelper carry _ [] = carry 
countColorPosHelper (count, carryCode, carryColors) (code:codes) (color:colors) = countColorPosHelper (count', carryCode', carryColors') codes colors
  where
    (count', carryCode', carryColors') = if code == color
                                           then (count+1, carryCode,      carryColors)
                                           else (count,   code:carryCode, color:carryColors)

deleteElem :: Eq a => a -> [a] -> [a]
deleteElem element [] = []
deleteElem element (a:as) = if a == element
                            then as
                            else [a] ++ deleteElem element as

interpretResult :: Int -> Code -> Board -> Int -> Maybe Result
interpretResult cols c b turn = 
  if correctness
    then Just Win
    else if isLastTurn
      then Just Lose
      else Nothing
  where
    correctness = isRowCorrect c b turn cols
    isLastTurn  = turn == rows

isRowCorrect :: Code -> Board -> Int -> Int -> Bool
isRowCorrect _    _ _   0   = True
isRowCorrect code b row col = 
  if correct
    then isRowCorrect code b row (col-1)
    else False
  where
    correct    = case maybeColor of
                  Nothing -> False
                  Just color -> color == codeColor
    maybeColor = boardLookup b (Pos row col)
    codeColor  = code !! (col-1)
