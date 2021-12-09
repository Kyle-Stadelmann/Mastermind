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
  --T.VtyEvent (V.EvKey (V.KChar 'm') _)  -> Brick.continue $ s {psCode = unsafePerformIO $ generateCode 4}
  T.VtyEvent (V.EvKey (V.KChar '-') _)  -> Brick.continue $ toggleDifficulty s
  T.VtyEvent (V.EvKey (V.KChar '=') _)  -> newGame s =<< liftIO (generateCode cols)
  T.VtyEvent (V.EvKey (V.KChar key) _)  -> inputCharKey s key
  T.VtyEvent (V.EvKey V.KLeft _)        -> Brick.continue (move left  s)
  T.VtyEvent (V.EvKey V.KRight _)       -> Brick.continue (move right s)
  T.VtyEvent (V.EvKey V.KEsc _)         -> Brick.halt s
  _                                     -> Brick.continue s

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

inputCharKey :: PlayState -> Char -> EventM n (Next PlayState)
inputCharKey s key = case maybeColor of
                  Just color -> Brick.continue s { psBoard = (insertColor board pos color) }
                  Nothing    -> Brick.continue s
  where
    maybeColor = M.lookup key keyColorMap
    board      = psBoard s
    pos        = psPos s


inputEnterKey :: PlayState -> EventM n (Next PlayState)
inputEnterKey s = 
  case result of
    Just _ ->
      let hints' = generateHints code board turn hints 
      in
      Brick.continue s {psHints = hints', psResult = result}
    Nothing ->
      -- generate hints
      let turn'  = turn + 1 
          pos'   = Pos turn' 1
          hints' = generateHints code board turn hints
      in    
      Brick.continue (s {psTurn = turn', psPos = pos', psHints = hints'})

  where
    result = interpretResult code board turn
    board  = psBoard s
    code   = psCode s
    turn   = psTurn s
    hints  = psHints s

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