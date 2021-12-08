module Model.Board where

import Prelude hiding (init)
import qualified Data.Map as M 

-------------------------------------------------------------------------------
-- | Main types/data structures -----------------------------------------------
-------------------------------------------------------------------------------

-- max dim of cols * rows
type Board = M.Map Pos Color

-- max dim of cols * rows
type Hints = M.Map Pos Hint

-- White and Black are purely hint colors
data Color = 
  Blue | Orange | Green | Red | Yellow | Pink | White | Black
  deriving (Eq, Show)
-- These are the playable colors (non hint colors)
allColors :: [Color]
allColors = [Blue, Orange, Green, Red, Yellow, Pink]

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

right :: Pos -> Pos 
right p = p 
  { pCol = min cols (pCol p + 1) 
  } 

insertColor :: Board -> Pos -> Color -> Board
insertColor b p c = M.insert p c b

insertHintRow :: Hints -> [Hint] -> Int -> Hints
insertHintRow currHints []     _   = currHints
insertHintRow currHints (h:hs) row = insertHintRow (M.insert pos h currHints) hs row
  where
    pos = Pos row col
    col = (cols - (length hs))

insertHint :: Hints -> Pos -> Hint -> Hints
insertHint hs p h = M.insert p h hs

-- Takes a maybe color and returns either the value or defaultColor if there is no value
maybeColorToColor :: Maybe Color -> Color
maybeColorToColor (Just v) = v
maybeColorToColor Nothing = defaultColor

-------------------------------------------------------------------------------
-- | Constants ----------------------------------------------------------------
-------------------------------------------------------------------------------
cols :: Int
cols = 4

rows :: Int
rows = 10

-- Default color of empty hint and main pegs
defaultColor :: Color
defaultColor = Black

keyColorMap :: M.Map Char Color
keyColorMap = M.fromList 
              [('b', Blue), 
               ('o', Orange),
               ('g', Green),
               ('r', Red),
               ('y', Yellow),
               ('p', Pink)
              ]