module Control where

import Brick hiding (Result)
import qualified Graphics.Vty as V
import qualified Brick.Types as T

import Model
import Model.Board
import Model.Computer
import Control.Monad.IO.Class (MonadIO(liftIO))

import Data.Map as M

control :: PlayState -> BrickEvent n Tick -> EventM n (Next PlayState)
control s ev = case ev of 
  T.AppEvent _                          -> Brick.continue $ handleTick s
  T.VtyEvent (V.EvKey V.KEnter _)       -> inputEnterKey s
  T.VtyEvent (V.EvKey (V.KChar '=') _)  -> newGame s =<< liftIO (generateCode (determineAllowDupes diff) cols)
  T.VtyEvent (V.EvKey (V.KChar '-') _)  -> let s'  = toggleDifficulty s 
                                               diff' = psDifficulty s'
                                               cols' = determineCols diff'
                                           in
                                           newGame s' =<< liftIO (generateCode (determineAllowDupes diff') cols')
  T.VtyEvent (V.EvKey V.KBS _)          -> Brick.continue (handleBackspace s)
  T.VtyEvent (V.EvKey (V.KChar key) _)  -> Brick.continue $ inputCharKey s key
  T.VtyEvent (V.EvKey V.KLeft _)        -> Brick.continue (move left  s)
  T.VtyEvent (V.EvKey V.KRight _)       -> Brick.continue (move (right cols) s)
  T.VtyEvent (V.EvKey V.KEsc _)         -> Brick.halt s
  _                                     -> Brick.continue s
  where
    diff = psDifficulty s
    cols = determineCols diff

-------------------------------------------------------------------------------
move :: (Pos -> Pos) -> PlayState -> PlayState
-------------------------------------------------------------------------------
move f s = if gameFinished
             then s
             else s'
  where
    s' = s { psPos = f (psPos s) }
    gameFinished = case result of
                     Just _ -> True
                     Nothing -> False
    result = psResult s    

inputCharKey :: PlayState -> Char -> PlayState
inputCharKey s key = 
  if gameFinished
    then s
    else case maybeColor of
           Just color -> s { psBoard = (insertColor board pos color) }
           Nothing    -> s
  where
    maybeColor = M.lookup key keyColorMap
    board      = psBoard s
    pos        = psPos s
    gameFinished = case result of
                     Just _ -> True
                     Nothing -> False
    result = psResult s    


inputEnterKey :: PlayState -> EventM n (Next PlayState)
inputEnterKey s = 
  case result of
    Just _ ->
      let hints' = generateHints cols code board turn hints 
      in
      Brick.continue s {psHints = hints', psResult = result}
    Nothing ->
      -- generate hints
      let turn'  = turn + 1 
          pos'   = Pos turn' 1
          hints' = generateHints cols code board turn hints
      in    
      Brick.continue (s {psTurn = turn', psPos = pos', psHints = hints'})

  where
    result = interpretResult cols code board turn
    board  = psBoard s
    code   = psCode s
    turn   = psTurn s
    hints  = psHints s
    cols   = determineCols diff
    diff   = psDifficulty s

handleTick :: PlayState -> PlayState
handleTick s = s {psTicks = ticks + 1}
  where
    ticks = psTicks s

-- Init all but the difficulty, and generate a new code
newGame :: PlayState -> Code -> EventM n (Next PlayState)
newGame s newCode = Brick.continue s'
  where 
    s'         = PS { psCode       = newCode
                    , psTurn       = 1
                    , psBoard      = initBoard
                    , psPos        = Pos 1 1 
                    , psHints      = initHints
                    , psResult     = Nothing
                    , psDifficulty = difficulty
                    , psTicks      = 0
                    }
    difficulty = psDifficulty s

handleBackspace :: PlayState -> PlayState
handleBackspace s = if gameFinished
                      then s
                      else s {psBoard = board'}
  where
    board' = insertColor board pos defaultColor
    pos = psPos s
    board = psBoard s
    gameFinished = case result of
                     Just _ -> True
                     Nothing -> False
    result = psResult s    