module Model.Board where

import Prelude hiding (init)
import qualified Data.Map as M 
import Data.Char (toLower)

-------------------------------------------------------------------------------
-- | Main types/data structures -----------------------------------------------
-------------------------------------------------------------------------------

-- max dim of cols * rows
type Board = M.Map Pos Color

-- max dim of cols * rows
type Hints = M.Map Pos Hint

-- White and Black are purely hint colors
data Color = 
  Blue | Orange | Green | Red | Yellow | Pink | Cyan | White | Black
  deriving (Eq, Show)
-- These are the playable colors (non hint colors)
allColors :: [Color]
allColors = [Blue, Orange, Green, Red, Yellow, Pink, Cyan]

-- max dim of cols
type Code = [Color]

data Pos = Pos 
  { pRow :: Int  -- 1 <= pRow <= rows 
  , pCol :: Int  -- 1 <= pCol <= cols
  }
  deriving (Eq, Ord, Show)

-- Correct color and pos | correct color | both incorrect
data Hint = ColorPos | Color | Incorrect
  deriving (Eq, Show)

data Result
  = Win
  | Lose
  deriving (Eq, Show)

data Difficulty
  = Easy | Medium | Hard
  deriving (Eq, Show)

data Stream a = a :| Stream a
  deriving (Show)

-------------------------------------------------------------------------------
-- | Helper functions ---------------------------------------------------------
-------------------------------------------------------------------------------
initBoard :: Board
initBoard = M.empty

initHints :: Hints
initHints = M.empty

boardLookup :: Board -> Pos -> Maybe Color 
boardLookup board pos = M.lookup pos board

hintLookup :: Hints -> Pos -> Maybe Hint 
hintLookup hints pos = M.lookup pos hints

colorInCode :: Code -> Color -> Bool
colorInCode code color = elem color code

left :: Pos -> Pos 
left p = p 
  { pCol   = max 1 (pCol p - 1) 
  } 

right :: Int -> Pos -> Pos 
right cols p = p 
  { pCol = min cols (pCol p + 1) 
  } 

insertColor :: Board -> Pos -> Color -> Board
insertColor b p c = M.insert p c b

insertHintRow :: Int -> Hints -> [Hint] -> Int -> Hints
insertHintRow _    currHints []     _   = currHints
insertHintRow cols currHints (h:hs) row = insertHintRow cols (M.insert pos h currHints) hs row
  where
    pos = Pos row col
    col = (cols - (length hs))

insertHint :: Hints -> Pos -> Hint -> Hints
insertHint hs p h = M.insert p h hs

-- Takes a maybe color and returns either the value or defaultColor if there is no value
maybeColorToColor :: Maybe Color -> Color
maybeColorToColor (Just v) = v
maybeColorToColor Nothing = defaultColor

firstLetterColor :: Color -> String
firstLetterColor color = [head (show color)]

-------------------------------------------------------------------------------
-- | Constants ----------------------------------------------------------------
-------------------------------------------------------------------------------
-- how fast the cursor changes from highlighted to not highlighted (in ticks)
cursorSpeed :: Int
cursorSpeed = 6

-- Default color of empty hint and main pegs
defaultColor :: Color
defaultColor = Black

keyColorMap :: M.Map Char Color
keyColorMap = M.fromList $ map (\color -> (toLower (head $ firstLetterColor color), color)) allColors

defaultCols :: Int
defaultCols = 4

defaultDiff :: Difficulty
defaultDiff = Easy

-- Max turns (cols defined in Model.hs)
rows :: Int
rows = 10

-- Number of pegs to guess, depends on the difficulty
determineCols :: Difficulty -> Int
determineCols Easy   = defaultCols
determineCols Medium = 5
determineCols Hard   = 5

-- Depending on difficulty allow duplicates in the code
determineAllowDupes :: Difficulty -> Bool
determineAllowDupes Easy   = False
determineAllowDupes Medium = False
determineAllowDupes Hard   = True