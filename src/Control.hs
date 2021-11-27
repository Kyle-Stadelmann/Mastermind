module Control where

import Brick hiding (Result)
import qualified Graphics.Vty as V
import qualified Brick.Types as T

import Model
import Model.Board
import Model.Computer

import Data.Map as M

control :: PlayState -> BrickEvent n Tick -> EventM n (Next PlayState)
control s ev = case ev of 
  T.VtyEvent (V.EvKey V.KEnter _)       -> inputEnterKey s
  T.VtyEvent (V.EvKey (V.KChar key) _)  -> inputCharKey s key
  T.VtyEvent (V.EvKey V.KLeft _)        -> Brick.continue (move left  s)
  T.VtyEvent (V.EvKey V.KRight _)       -> Brick.continue (move right s)
  T.VtyEvent (V.EvKey V.KEsc _)         -> Brick.halt s
  _                                     -> Brick.continue s -- Brick.halt s

-------------------------------------------------------------------------------
move :: (Pos -> Pos) -> PlayState -> PlayState
-------------------------------------------------------------------------------
move f s = s { psPos = f (psPos s) }

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
    Just winlose ->
      let s' = s {psResult = result} 
      in
      -- display victory screen
      Brick.continue s'
    Nothing ->
      -- generate hints
      let turn' = turn + 1 
          pos'  = Pos turn' 1
      in    
      Brick.continue (s {psTurn = turn', psPos = pos'})

  where
    result = interpretResult code board turn
    board  = psBoard s
    code   = psCode s
    turn   = psTurn s

{-
-------------------------------------------------------------------------------

control :: PlayState -> BrickEvent n Tick -> EventM n (Next PlayState)
control s ev = case ev of 
  AppEvent Tick                   -> nextS s =<< liftIO (play O s)
  T.VtyEvent (V.EvKey V.KEnter _) -> nextS s =<< liftIO (play X s)

-------------------------------------------------------------------------------
play :: XO -> PlayState -> IO (Result Board)
-------------------------------------------------------------------------------
play xo s
  | psTurn s == xo = put (psBoard s) xo <$> getPos xo s 
  | otherwise      = return Retry

getPos :: XO -> PlayState -> IO Pos
getPos xo s = getStrategy xo s (psPos s) (psBoard s) xo

getStrategy :: XO -> PlayState -> Strategy 
getStrategy X s = plStrat (psX s)
getStrategy O s = plStrat (psO s)

-------------------------------------------------------------------------------
nextS :: PlayState -> Result Board -> EventM n (Next PlayState)
-------------------------------------------------------------------------------
nextS s b = case next s b of
  Right s' -> continue s'
  Left res -> halt (s { psResult = res }) 
-}

