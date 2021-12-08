module View where

import Brick
import Brick.Widgets.Center (center, hCenter, hCenterLayer, hCenterWith, centerAbout)
import Brick.Widgets.Border (border, borderWithLabel, hBorder, vBorder)
import Brick.Widgets.Border.Style (unicode)
import Text.Printf (printf)

import Model
import Model.Board as B
import Graphics.Vty as V hiding (dim)
import Model.Computer

-------------------------------------------------------------------------------
view :: PlayState -> [Widget String]
-------------------------------------------------------------------------------
view s = [center $ viewHelper s]

viewHelper :: PlayState -> Widget n
viewHelper s = (padRight (Pad 8) makeAvailColors)
               <+> (makeMainBoardWithBorder s <=> (endScreenText s))
               <+> (padLeft (Pad 8) makeControlBox)


-------------------------------------------------------------------------------
-- | Main board ---------------------------------------------------------------
-------------------------------------------------------------------------------
makeMainBoardWithBorder :: PlayState -> Widget n
makeMainBoardWithBorder s = ((borderWithLabel (str (mainHeader s))) $ (makeMainBoard s))

makeMainBoard :: PlayState -> Widget n
makeMainBoard s = vBox $ (makeCodeRow s):[makeRow s row | row <- [rows,rows-1..1]]

makeRow :: PlayState -> Int -> Widget n
makeRow s r = makeLeftHalfHintRow s r <+> makePlayerRow s r <+> makeRightHalfHintRow s r

makePlayerRow :: PlayState -> Int -> Widget n
makePlayerRow s r = padLeftRight 1 $ border $ hBox [padLeftRight 1 (makePeg s r c) | c <- [1..cols]]

makePeg :: PlayState -> Int -> Int -> Widget n
makePeg s r c  
  | isCurr s r c && (not gameOver) = withCursor raw
  | otherwise    = raw
  where
    raw        = makeColorPeg maybeColor
    maybeColor = boardLookup board pos
    pos        = Pos r c
    board      = psBoard s
    gameOver   = result == (Just Win) || result == (Just Lose)
    result     = psResult s

makeColorPeg :: Maybe B.Color -> Widget n
makeColorPeg maybeColor = border $ withAttr (colorToAttr color) peg
  where
    color = case maybeColor of
              Just c -> c
              _      -> defaultColor

peg :: Widget n
peg = str "  "

-------------------------------------------------------------------------------
-- | Avail colors box ---------------------------------------------------------
-------------------------------------------------------------------------------
makeAvailColors :: Widget n
makeAvailColors =   
   borderWithLabel (str availHeader)
   $ availColorBox

availColorBox :: Widget n
availColorBox = 
  border $
    hBox [makeColorPeg (Just c) | c <- allColors]

-------------------------------------------------------------------------------
-- | End Screen Text ----------------------------------------------------------
-------------------------------------------------------------------------------
endScreenText :: PlayState -> Widget n
endScreenText s = text
  where
    text   = case result of
               Just Win     -> withAttr victoryAttr (str "Congratulations, you won! Start a new game with \"=\"")
               Just Lose    -> withAttr loseAttr    (str "Sorry, you lost! Start a new game with \"=\"")
               Nothing -> str ""
    result = psResult s
    
-------------------------------------------------------------------------------
-- | Control box --------------------------------------------------------------
-------------------------------------------------------------------------------
makeControlBox :: Widget n
makeControlBox = borderWithLabel (str controlHeader) $ makeControls

makeControls :: Widget n
makeControls = hLimit 30
               $ vBox [ makeControl "Left" "←"
                      , makeControl "Right" "→"
                      , makeControl "End turn" "Enter"
                      , makeControl "End game" "Esc"
                      , makeControl "New game" "="
                      ]

makeControl :: String -> String -> Widget n
makeControl function key = str function 
                           <+> (padLeft Max $ str key)

-------------------------------------------------------------------------------
-- | Secret code row ----------------------------------------------------------
-------------------------------------------------------------------------------
makeCodeRow :: PlayState -> Widget n
makeCodeRow s = padAll 2 (str "Hints")
                <+> (padLeftRight 4 (border $ hBox [padLeftRight 1 (makeCodePeg s c) | c <- [0..cols-1]]))
                <+> padAll 2 (str "Hints")

makeCodePeg :: PlayState -> Int -> Widget n
makeCodePeg s c = peg
  where
    peg   = case maybeResult of
                    -- Game over; display the previously hidden code
                    Just _  -> makeColorPeg (maybeColor)
                    -- Active game still going; no result yet; display hiddenCodePeg
                    Nothing -> border $ withAttr (colorToAttr Black) hiddenCodePeg
    maybeResult = psResult s
    code = psCode s
    maybeColor = Just (code !! c)


hiddenCodePeg :: Widget n
hiddenCodePeg = str "??"

-------------------------------------------------------------------------------
-- | Hint rows ----------------------------------------------------------------
-------------------------------------------------------------------------------
makeLeftHalfHintRow :: PlayState -> Int -> Widget n
makeLeftHalfHintRow s r = padRight (Pad 2) $ border
                          $ hBox [(makeHintPeg s r c) | c <- [1..cols `div` 2]]

makeRightHalfHintRow :: PlayState -> Int -> Widget n
makeRightHalfHintRow s r = padLeft (Pad 2) $ border
                            $ hBox [(makeHintPeg s r c) | c <- [(cols `div` 2 + 1)..cols]]

makeHintPeg :: PlayState -> Int -> Int -> Widget n
makeHintPeg s r c = makeColorPeg color
  where
    color = case hint of
              Just ColorPos  -> Just Red
              Just Color     -> Just White
              _              -> Nothing
    hint = hintLookup hints pos
    pos = Pos r c
    hints = psHints s

-------------------------------------------------------------------------------
-- | Util ---------------------------------------------------------------------
-------------------------------------------------------------------------------
mainHeader :: PlayState -> String
mainHeader s = printf "Mastermind Turn = %s, row = %d, col = %d" (show (psTurn s)) (pRow p) (pCol p)
  where 
    p    = psPos s

availHeader :: String
availHeader = "Available Colors"

controlHeader :: String
controlHeader = "Controls"

withCursor :: Widget n -> Widget n
withCursor = modifyDefAttr (`withStyle` reverseVideo)

-------------------------------------------------------------------------------
-- | Colors -------------------------------------------------------------------
-------------------------------------------------------------------------------
colorToAttr :: B.Color -> AttrName
colorToAttr Blue = blueAttr
colorToAttr Orange = orangeAttr
colorToAttr Green = greenAttr
colorToAttr Red = redAttr
colorToAttr Yellow = yellowAttr
colorToAttr Pink = pinkAttr
colorToAttr White = whiteAttr
colorToAttr Black = blackAttr

-------------------------------------------------------------------------------
-- | Attributes ---------------------------------------------------------------
-------------------------------------------------------------------------------
blueAttr, orangeAttr, greenAttr, redAttr, yellowAttr, pinkAttr, blackAttr, whiteAttr, victoryAttr, loseAttr :: AttrName
blueAttr = attrName "blue"
orangeAttr = attrName "orange"
greenAttr = attrName "green"
redAttr = attrName "red"
yellowAttr = attrName "yellow"
pinkAttr = attrName "pink"
blackAttr = attrName "black"
whiteAttr = attrName "white"
victoryAttr = attrName "victory"
loseAttr = attrName "lose"

attributeMap :: AttrMap
attributeMap = attrMap
  V.defAttr
  [ 
    (blueAttr, bg V.blue),
    (orangeAttr, bg $ V.rgbColor 255 140 0),
    (greenAttr, bg V.green),
    (redAttr, bg V.red),
    (yellowAttr, bg $ V.rgbColor 255 255 0),
    (pinkAttr, bg $ V.rgbColor 255 153 255),
    (blackAttr, bg V.black),
    (whiteAttr, bg V.white),
    (victoryAttr, fg V.green),
    (loseAttr, fg V.red)
  ]
