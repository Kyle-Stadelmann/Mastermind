module Main where 

import Test.Tasty
import Common

import Model
import Model.Board
import Model.Computer
import Data.List

main :: IO ()
main = do 
  putStrLn "\nRunning my tests... "
  runTests [ testGenerateHints1,
             testDetermineCols1,
             testInterpretResult1,
             testInsertHintRow1,
             testDetermineAllowDupes1,
             testGenerateCodeNoDupes1,
             testGenerateHints2,
             testDetermineCols2,
             testInterpretResult2,
             testInsertHintRow2,
             testDetermineAllowDupes2,
             testGenerateCodeDupes2,
             testGenerateHints3,
             testDetermineCols3,
             testInterpretResult3,
             testInsertHintRow3,
             testDetermineAllowDupes3,
             testGenerateCodeNoDupes3,
             testGenerateHints4,
             testDetermineCols4,
             testInterpretResult4,
             testInsertHintRow4,
             testDetermineAllowDupes4,
             testGenerateCodeDupes4
           ]
  putStrLn "\nDone Testing"

-------------------------------------------------------------------------------
-- | Test objects -------------------------------------------------------------
-------------------------------------------------------------------------------
code1 :: Code
code1 = [Red, Green, Blue, Orange]

-- first row: [Red, Pink, Blue, Green]
board1 :: Board
board1 = insert4
  where
    insert4 = insertColor insert3 (Pos 1 4) Green
    insert3 = insertColor insert2 (Pos 1 3) Blue
    insert2 = insertColor insert1 (Pos 1 2) Pink
    insert1 = insertColor init (Pos 1 1) Red
    init = initBoard

difficulty1 :: Difficulty
difficulty1 = Easy

cols1 :: Int
cols1 = 4

allowDupes1 :: Bool
allowDupes1 = False

hints1 :: Hints
hints1 = actual4
  where
    -- 2 ColorPos, 1 Color, 1 Incorrectt
    actual4 = insertHint actual3 (Pos 1 4) Incorrect
    actual3 = insertHint actual2 (Pos 1 3) Color
    actual2 = insertHint actual1 (Pos 1 2) ColorPos
    actual1 = insertHint actualInit (Pos 1 1) ColorPos
    actualInit = initHints

hintList1 :: [Hint]
hintList1 = [ColorPos, ColorPos, Color, Incorrect]


code2 :: Code
code2 = [Green, Green, Yellow, Orange, Cyan]

-- third row: [Green, Orange, Yellow, Green, Green]
board2 :: Board
board2 = insert5
  where
    insert5 = insertColor insert4 (Pos 3 5) Green
    insert4 = insertColor insert3 (Pos 3 4) Green
    insert3 = insertColor insert2 (Pos 3 3) Yellow
    insert2 = insertColor insert1 (Pos 3 2) Orange
    insert1 = insertColor init (Pos 3 1) Green
    init = initBoard

difficulty2 :: Difficulty
difficulty2 = Hard

cols2 :: Int
cols2 = 5

allowDupes2 :: Bool
allowDupes2 = True

hints2 :: Hints
hints2 = actual5
  where
    -- 2 ColorPos, 1 Color, 1 Incorrect
    actual5 = insertHint actual4 (Pos 3 5) Incorrect
    actual4 = insertHint actual3 (Pos 3 4) Color
    actual3 = insertHint actual2 (Pos 3 3) Color
    actual2 = insertHint actual1 (Pos 3 2) ColorPos
    actual1 = insertHint actualInit (Pos 3 1) ColorPos
    actualInit = initHints

hintList2 :: [Hint]
hintList2 = [ColorPos, ColorPos, Color, Color, Incorrect]


code3 :: Code
code3 = [Blue, Green, Cyan, Pink, Red]

-- tenth row: [Green, Black, Yellow, Black, Green]
board3 :: Board
board3 = insert5
  where
    insert5 = insertColor insert3 (Pos 10 5) Green
    insert3 = insertColor insert1 (Pos 10 3) Yellow
    insert1 = insertColor init (Pos 10 1) Green
    init = initBoard

difficulty3 :: Difficulty
difficulty3 = Medium

cols3 :: Int
cols3 = 5

allowDupes3 :: Bool
allowDupes3 = False

hints3 :: Hints
hints3 = actual5
  where
    -- 0 ColorPos, 1 Color, 4 Incorrect
    actual5 = insertHint actual4 (Pos 10 5) Incorrect
    actual4 = insertHint actual3 (Pos 10 4) Incorrect
    actual3 = insertHint actual2 (Pos 10 3) Incorrect
    actual2 = insertHint actual1 (Pos 10 2) Incorrect
    actual1 = insertHint actualInit (Pos 10 1) Color
    actualInit = initHints

hintList3 :: [Hint]
hintList3 = [Color, Incorrect, Incorrect, Incorrect, Incorrect]


code4 :: Code
code4 = [Blue, Green, Blue, Cyan, Cyan]

-- tenth row: [Blue, Green, Blue, Cyan, Cyan]
board4 :: Board
board4 = insert5
  where
    insert5 = insertColor insert4 (Pos 10 5) Cyan
    insert4 = insertColor insert3 (Pos 10 4) Cyan
    insert3 = insertColor insert2 (Pos 10 3) Blue
    insert2 = insertColor insert1 (Pos 10 2) Green
    insert1 = insertColor init (Pos 10 1) Blue
    init = initBoard

difficulty4 :: Difficulty
difficulty4 = Hard

cols4 :: Int
cols4 = 5

allowDupes4 :: Bool
allowDupes4 = True

hints4 :: Hints
hints4 = actual5
  where
    -- 5 ColorPos, 0 Color, 0 Incorrect
    actual5 = insertHint actual4 (Pos 10 5) ColorPos
    actual4 = insertHint actual3 (Pos 10 4) ColorPos
    actual3 = insertHint actual2 (Pos 10 3) ColorPos
    actual2 = insertHint actual1 (Pos 10 2) ColorPos
    actual1 = insertHint actualInit (Pos 10 1) ColorPos
    actualInit = initHints

hintList4 :: [Hint]
hintList4 = [ColorPos, ColorPos, ColorPos, ColorPos, ColorPos]

-------------------------------------------------------------------------------
-- | Tests --------------------------------------------------------------------
-------------------------------------------------------------------------------
testGenerateHints1 :: Score -> TestTree
testGenerateHints1 sc = mkTest' sc (\_ -> return $ res) () actual "testGenerateHints1"
  where
    res = generateHints cols1 code1 board1 1 initHints
    actual = hints1

testDetermineCols1 :: Score -> TestTree
testDetermineCols1 sc = mkTest' sc (\_ -> return $ res) () actual "testDetermineCols1"
  where
    res = determineCols difficulty1
    actual = cols1

testInterpretResult1 :: Score -> TestTree
testInterpretResult1 sc = mkTest' sc (\_ -> return $ res) () actual "testInterpretResult1"
  where
    res = interpretResult cols1 code1 board1 1
    actual = Nothing

testInsertHintRow1 :: Score -> TestTree
testInsertHintRow1 sc = mkTest' sc (\_ -> return $ res) () actual "testInsertHintRow1"
  where
    res = insertHintRow cols1 initHints hintList1 1
    actual = hints1

testDetermineAllowDupes1 :: Score -> TestTree
testDetermineAllowDupes1 sc = mkTest' sc (\_ -> return $ res) () actual "testInsertHintRow1"
  where
    res = determineAllowDupes difficulty1
    actual = allowDupes1

-- Checks if there are no dupes in the generated code and that the code is cols1 colors long
testGenerateCodeNoDupes1 :: Score -> TestTree
testGenerateCodeNoDupes1 sc = mkTest' sc (\_ -> res) () actual "testGenerateCodeNoDupes1"
  where
    res = do 
            code <- generateCode allowDupes1 cols1
            return $ length (nub code)
    actual = cols1

testGenerateHints2 :: Score -> TestTree
testGenerateHints2 sc = mkTest' sc (\_ -> return $ res) () actual "testGenerateHints2"
  where
    res = generateHints cols2 code2 board2 3 initHints
    actual = hints2

testDetermineCols2 :: Score -> TestTree
testDetermineCols2 sc = mkTest' sc (\_ -> return $ res) () actual "testDetermineCols2"
  where
    res = determineCols difficulty2
    actual = cols2

testInterpretResult2 :: Score -> TestTree
testInterpretResult2 sc = mkTest' sc (\_ -> return $ res) () actual "testInterpretResult2"
  where
    res = interpretResult cols2 code2 board2 3
    actual = Nothing

testInsertHintRow2 :: Score -> TestTree
testInsertHintRow2 sc = mkTest' sc (\_ -> return $ res) () actual "testInsertHintRow2"
  where
    res = insertHintRow cols2 initHints hintList2 3
    actual = hints2

testDetermineAllowDupes2 :: Score -> TestTree
testDetermineAllowDupes2 sc = mkTest' sc (\_ -> return $ res) () actual "testDetermineAllowDupes2"
  where
    res = determineAllowDupes difficulty2
    actual = allowDupes2

-- Checks if there are dupes in a generated code and that the code is cols2 colors long
-- try until we get a code with a dupe (will timeout eventually if dupes can't generated)
testGenerateCodeDupes2 :: Score -> TestTree
testGenerateCodeDupes2 sc = mkTest' sc (\_ -> getDupe) () True "testGenerateCodeNoDupes2"


testGenerateHints3 :: Score -> TestTree
testGenerateHints3 sc = mkTest' sc (\_ -> return $ res) () actual "testGenerateHints3"
  where
    res = generateHints cols3 code3 board3 10 initHints
    actual = hints3

testDetermineCols3 :: Score -> TestTree
testDetermineCols3 sc = mkTest' sc (\_ -> return $ res) () actual "testDetermineCols3"
  where
    res = determineCols difficulty3
    actual = cols3

testInterpretResult3 :: Score -> TestTree
testInterpretResult3 sc = mkTest' sc (\_ -> return $ res) () actual "testInterpretResult3"
  where
    res = interpretResult cols3 code3 board3 10
    actual = Just Lose

testInsertHintRow3 :: Score -> TestTree
testInsertHintRow3 sc = mkTest' sc (\_ -> return $ res) () actual "testInsertHintRow3"
  where
    res = insertHintRow cols3 initHints hintList3 10
    actual = hints3

testDetermineAllowDupes3 :: Score -> TestTree
testDetermineAllowDupes3 sc = mkTest' sc (\_ -> return $ res) () actual "testDetermineAllowDupes3"
  where
    res = determineAllowDupes difficulty3
    actual = allowDupes3

-- Checks if there are no dupes in the generated code and that the code is cols1 colors long
testGenerateCodeNoDupes3 :: Score -> TestTree
testGenerateCodeNoDupes3 sc = mkTest' sc (\_ -> res) () actual "testGenerateCodeNoDupes3"
  where
    res = do 
            code <- generateCode allowDupes3 cols3
            return $ length (nub code)
    actual = cols3


testGenerateHints4 :: Score -> TestTree
testGenerateHints4 sc = mkTest' sc (\_ -> return $ res) () actual "testGenerateHints4"
  where
    res = generateHints cols4 code4 board4 10 initHints
    actual = hints4

testDetermineCols4 :: Score -> TestTree
testDetermineCols4 sc = mkTest' sc (\_ -> return $ res) () actual "testDetermineCols4"
  where
    res = determineCols difficulty4
    actual = cols4

testInterpretResult4 :: Score -> TestTree
testInterpretResult4 sc = mkTest' sc (\_ -> return $ res) () actual "testInterpretResult4"
  where
    res = interpretResult cols4 code4 board4 10
    actual = Just Win

testInsertHintRow4 :: Score -> TestTree
testInsertHintRow4 sc = mkTest' sc (\_ -> return $ res) () actual "testInsertHintRow4"
  where
    res = insertHintRow cols4 initHints hintList4 10
    actual = hints4

testDetermineAllowDupes4 :: Score -> TestTree
testDetermineAllowDupes4 sc = mkTest' sc (\_ -> return $ res) () actual "testDetermineAllowDupes4"
  where
    res = determineAllowDupes difficulty4
    actual = allowDupes4

-- Checks if there are no dupes in the generated code and that the code is cols1 colors long
testGenerateCodeDupes4 :: Score -> TestTree
testGenerateCodeDupes4 sc = mkTest' sc (\_ -> getDupe) () True "testGenerateCodeDupes4"

-------------------------------------------------------------------------------
-- | Test Util ----------------------------------------------------------------
-------------------------------------------------------------------------------
getDupe :: IO Bool
getDupe =
  do 
    code <- generateCode allowDupes2 cols2
    if (length (nub code)) == (length code)
      then getDupe
      else return True