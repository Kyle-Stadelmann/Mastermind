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
  deriving (Eq, Ord)

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