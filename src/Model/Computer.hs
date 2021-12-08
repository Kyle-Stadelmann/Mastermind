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
    hintList         = [getHintByPos hintCounts row col | col <- [1..cols]]
    hintCounts       = foldr countHintTypes (0,0,0) hintListUnsorted
    hintListUnsorted = [generateHint code b row col | col <- [1..cols]]

generateHint :: Code -> Board -> Int -> Int -> Hint
generateHint code b row col = 
  case actualColorMaybe of
    Nothing           -> Incorrect
    Just actualColor  -> compareWithCode code actualColor col
  where
    actualColorMaybe   = (boardLookup b (Pos row col))

compareWithCode :: Code -> Color -> Int -> Hint
compareWithCode code color col
  | color == expectedColor = ColorPos
  | colorInCode code color = Color
  | True                   = Incorrect
  where
    -- minus 1 because we are 1-based indexing for pos
    expectedColor = (code !! (col-1))

-- Returns count of (ColorPos, Color, Incorrect) tuple
countHintTypes :: Hint -> (Int, Int, Int) -> (Int, Int, Int)
countHintTypes ColorPos  (cp,c,i) = (cp+1,c,i)
countHintTypes Color     (cp,c,i) = (cp,c+1,i)
countHintTypes Incorrect (cp,c,i) = (cp,c,i+1)

-- Inserts hints based off (ColorPos, Color, Incorrect) tuple
getHintByPos :: (Int, Int, Int) -> Int -> Int -> Hint
getHintByPos (cp,c,i) row col 
  | cp >= col       = ColorPos
  | (cp + c) >= col = Color
  | True            = Incorrect

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
    correct = hint == ColorPos
    hint    = generateHint code b row col