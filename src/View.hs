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
import System.IO.Unsafe (unsafePerformIO)

-------------------------------------------------------------------------------
view :: PlayState -> [Widget String]
-------------------------------------------------------------------------------
view s = [center $ viewHelper s]

viewHelper :: PlayState -> Widget n
viewHelper s = joinBorders 
               $ (borderWithLabel (str (header s))) 
               $ vBox [makeMainBoard s, hLimit 52 hBorder, makeAvailColors]


-------------------------------------------------------------------------------
-- | Main board ---------------------------------------------------------------
-------------------------------------------------------------------------------
makeMainBoard :: PlayState -> Widget n
makeMainBoard s = vBox $ (makeCodeRow s):[makeRow s row | row <- [rows,rows-1..1]]

makeRow :: PlayState -> Int -> Widget n
makeRow s r = makeLeftHalfHintRow s r <+> makePlayerRow s r <+> makeRightHalfHintRow s r

makePlayerRow :: PlayState -> Int -> Widget n
makePlayerRow s r = padLeftRight 1 $ border $ hBox [padLeftRight 1 (makePeg s r c) | c <- [1..cols]]

makePeg :: PlayState -> Int -> Int -> Widget n
makePeg s r c  
  | isCurr s r c = withCursor raw
  | otherwise    = raw
  where
    raw        = makeColorPeg maybeColor
    maybeColor = boardLookup board pos
    pos        = Pos r c
    board      = psBoard s

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
   (padLeft (Pad 18) $ str "Available Colors")
    <=> (padLeft (Pad 13) availColorBox)

availColorBox :: Widget n
availColorBox = 
  border $
    hBox [makeColorPeg (Just c) | c <- allColors]

-------------------------------------------------------------------------------
-- | Secret code row ----------------------------------------------------------
-------------------------------------------------------------------------------
makeCodeRow :: PlayState -> Widget n
makeCodeRow s = padAll 2 (str "Hints")
                <+> (padLeftRight 4 (border $ hBox [padLeftRight 1 (makeCodePeg s c) | c <- [0..cols-1]]))
                <+> padAll 2 (str "Hints")

makeCodePeg :: PlayState -> Int -> Widget n
makeCodePeg s c = border $ peg
  where
    peg   = case maybeResult of
                    -- Game over; display the previously hidden code
                    Just _  -> makeColorPeg (maybeColor)
                    -- Active game still going; no result yet; display hiddenCodePeg
                    Nothing -> withAttr (colorToAttr Black) hiddenCodePeg
    maybeResult = psResult s
    code = unsafePerformIO (psCode s)
    maybeColor = Just (code !! c)


hiddenCodePeg :: Widget n
hiddenCodePeg = str "??"

-------------------------------------------------------------------------------
-- | Hint rows ----------------------------------------------------------------
-------------------------------------------------------------------------------
makeLeftHalfHintRow :: PlayState -> Int -> Widget n
makeLeftHalfHintRow s r = padRight (Pad 2) $ border
                          $ hBox [(makeHintPeg s r c) | c <- [cols,cols-1..(cols `div` 2 + 1)]]

makeRightHalfHintRow :: PlayState -> Int -> Widget n
makeRightHalfHintRow s r = padLeft (Pad 2) $ border
                            $ hBox [(makeHintPeg s r c) | c <- [cols `div` 2,(cols `div` 2)-1..1]]

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
header :: PlayState -> String
header s = printf "Mastermind Turn = %s, row = %d, col = %d" (show (psTurn s)) (pRow p) (pCol p)
  where 
    p    = psPos s

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
blueAttr, orangeAttr, greenAttr, redAttr, yellowAttr, pinkAttr, blackAttr, whiteAttr :: AttrName
blueAttr = attrName "blue"
orangeAttr = attrName "orange"
greenAttr = attrName "green"
redAttr = attrName "red"
yellowAttr = attrName "yellow"
pinkAttr = attrName "pink"
blackAttr = attrName "black"
whiteAttr = attrName "white"

attributeMap :: AttrMap
attributeMap = attrMap
  V.defAttr
  [ 
    (blueAttr, bg V.blue),
    (orangeAttr, bg $ V.rgbColor 255 153 51),
    (greenAttr, bg V.green),
    (redAttr, bg V.red ),
    (yellowAttr, bg V.yellow),
    (pinkAttr, bg $ V.rgbColor 255 153 255),
    (blackAttr, bg V.black),
    (whiteAttr, bg V.white)
  ]