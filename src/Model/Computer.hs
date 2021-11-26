module Model.Computer where

import Model.Board
import System.Random -- (Random(randomRIO))

-------------------------------------------------------------------------------
-- | Helper functions taken by opponent player (computer) ---------------------
-------------------------------------------------------------------------------
generateCode :: IO Code
generateCode = generateCodeHelper colors [] cols
  where
    colors = allColors

generateCodeHelper :: [Color] -> [Color] -> Int -> IO [Color]
generateCodeHelper _ currColors 0 = return currColors
generateCodeHelper allColors currColors numColors = 
  do
    newCurrColors <- generateColorNoDupes allColors currColors
    generateCodeHelper allColors newCurrColors (numColors - 1)


generateColorNoDupes :: [Color] -> [Color] -> IO [Color]
generateColorNoDupes allColors currColors = 
  do
    colorIndex <- randomRIO (0, length allColors - 1)
    let color = allColors !! colorIndex
    if elem color currColors
      then generateColorNoDupes allColors currColors
      else return (color : currColors)
