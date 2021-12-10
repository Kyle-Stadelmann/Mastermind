module Model where

import Prelude hiding ((!!))
import Model.Board
import Model.Computer

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
  { psCode       :: Code            -- generated answer color code
  , psTurn       :: Int                   -- what turn we are on (current row)
  , psBoard      :: Board           -- current board (player rows)
  , psPos        :: Pos             -- current cursor (within the current row)
  , psHints      :: Hints           -- current hints given thus far
  , psResult     :: Maybe Result    -- game result
  , psDifficulty :: Difficulty      -- game difficulty
  , psTicks      :: Int                   -- how many ticks have passed since game started
  } 


init :: IO PlayState
init = 
  do 
    code <- generateCode (determineAllowDupes defaultDiff) defaultCols
    let ps = PS 
              { psCode       = code
              , psTurn       = 1
              , psBoard      = initBoard
              , psPos        = Pos 1 1 
              , psHints      = initHints
              , psResult     = Nothing
              , psDifficulty = Easy
              , psTicks      = 0
              }
    return ps

isCurr :: PlayState -> Int -> Int -> Bool
isCurr s r c = pRow p == r && pCol p == c
  where 
    p = psPos s 

-- Move to next difficulty
-- ends the game, starts a new one with new difficulty
toggleDifficulty :: PlayState -> PlayState
toggleDifficulty s = s {psDifficulty = diff'}
  where
    diff' = case diff of
              Easy -> Medium
              Medium -> Hard
              Hard -> Easy
    diff  = psDifficulty s

debugMode :: Bool
debugMode = False