module Model where

import Prelude hiding ((!!))
import qualified Model.Board as Board
import Model.Computer
import System.IO.Unsafe (unsafePerformIO)

-------------------------------------------------------------------------------
-- | Ticks mark passing of time: a custom event that we constantly stream
-------------------------------------------------------------------------------
data Tick = Tick

-------------------------------------------------------------------------------
-- | Top-level App State ------------------------------------------------------
-------------------------------------------------------------------------------

data State 
  = Intro 
  | Play PlayState 
  | Outro 
  
data PlayState = PS
  { psCodeStream :: Stream Board.Code     -- Infinite stream of codes, for future games
  , psCode       :: Board.Code            -- generated answer color code
  , psTurn       :: Int                   -- what turn we are on (current row)
  , psBoard      :: Board.Board           -- current board (player rows)
  , psPos        :: Board.Pos             -- current cursor (within the current row)
  , psHints      :: Board.Hints           -- current hints given thus far
  , psResult     :: Maybe Board.Result    -- game result
  , psDifficulty :: Board.Difficulty      -- game difficulty
  , psTicks      :: Int                   -- how many ticks have passed since game started
  } 


init :: IO PlayState
init = PS 
  { psCodeStream = 
  , psCode       = unsafePerformIO $ generateCode Board.cols
  , psTurn       = 1
  , psBoard      = Board.initBoard
  , psPos        = Board.Pos 1 1 
  , psHints      = Board.initHints
  , psResult     = Nothing
  , psDifficulty = Board.Easy
  , psTicks      = 0
  }

isCurr :: PlayState -> Int -> Int -> Bool
isCurr s r c = Board.pRow p == r && Board.pCol p == c
  where 
    p = psPos s 

-- Init all but the difficulty, newcode
newGame :: PlayState -> PlayState
newGame s = Model.init {psDifficulty = difficulty, psCode = newCode}
  where 
    difficulty = psDifficulty s
    newCode    = unsafePerformIO $ generateCode Board.cols

-- Move to next difficulty
toggleDifficulty :: PlayState -> PlayState
toggleDifficulty s = s {psDifficulty = diff'}
  where
    diff' = case diff of
              Board.Easy -> Board.Medium
              Board.Medium -> Board.Hard
              Board.Hard -> Board.Easy
    diff  = psDifficulty s

debugMode :: Bool
debugMode = True
