# Mastermind
For my CSE 230 final project I intend to develop the game [Mastermind](https://en.wikipedia.org/wiki/Mastermind_(board_game)) in Haskell using the [brick library](https://github.com/jtdaugherty/brick/). 

## Quick Game Overview (Actual Proposal)
Mastermind is a 2-player puzzle game where one player is the "codemaker" and the other the "codebreaker". While there are multiple variations of the rules/board, the one I will be implementing consists of 10 rows of 4 peg slots each. Pegs come in 6 different colors. The codemaker (the computer) will choose an order of 4 different colored pegs, and the player's goal is to guess this combination. The player has 10 attempts at doing so. Each attempt will result in the computer giving hints. The hints work as follows: for every correctly placed colored-peg, one red 'key-peg' is displayed, and for every other colored-peg that contains a correct color but wrong position, one white 'key-peg' is displayed. The player uses logic based on these key-pegs to make their next guess. The player wins if they guess correctly on or before their 10th attempt.

## Timeline/Explanation (Extra Information)
I didn't realize until a couple days before the proposal deadline that the final project wasn't required to be done indvidually for comprehensive exam students, and I don't know anyone in this course so I wasn't able to find a group in time (extremely sorry about this).
Nonetheless, I would like to embark on this project solo due to various reasons (I hope with the following justification/timeline, to gain permission). My past relevant coursework includes CSE 130, 131, 231 and now 230. In CSE 231, I participated in a group project at the end of the course [link to my group's repo here](https://github.com/R167/chocopy-wasm-compiler). This project was fun/interesting, but I found it somewhat dissapointing as I couldn't contribute as much as I wanted to due to the lack of available work combined with the group setting. So in order to make this a project I'm more proud of (and fully understand the intricacies of) I'd like to attempt it individually. I'm prepared to put in the extra hours/work to make this a successful project- as this is the kind of side-project that I would enjoy outside of school anyway. Thank you for the consideration, and I deeply apologize for not finding a group in time.

I planned the following schedule (around other responsibilities) to keep myself in check and to ensure success in implementing this project. Reasonably small goals were placed every few days to allow for adjustments to the timeline if necessary, but overall, to ensure steady progress. I left extra time towards the end for wrapping up, secondary, and stretch goals. This time will act as a buffer to ensure the primary goals were accomplished before continuing on to the lower priority ones.
| Feature                                                                    | Target Date   | Completed |
| -------------------------------------------------------------------------- | ------------- | ----------|
| Repo creation/setup                                                        | 11/15/21      |     ✔
| Data structures (and state) planning/setup                                 | 11/17/21      |     ✔
| Basic UI setup                                                             | 11/20/21      |     ✔
| Helper functions to manipulate UI (state) using data structures            | 11/22/21      |     ✔
| Helper functions to take input and display on board                        | 11/24/21      |     ✔
| Game logic functions: generate code + generate key pegs based off input    | 11/27/21      |     ✔
| Game logic functions: increment round                                      | 12/1/21       |     ✔
| Wrap up: confirm easy installation on other environments, unit tests, etc. | 12/4/21       |     ✔
| Stretch/secondary goals: win/lose screens, restart buttons, etc.           | 12/7/21       |     ✔
| Prepare for presentation                                                   | 12/9/21       |     ✔

## Goals
### Primary Goals
- Implement at least a basic form of mastermind using 10 rows, 6 color pegs (using letter to denote color), no duplicate colors (for less complexity)
- Basic input (type first letter of color in order, enter to confirm, backspace to change)
- Simply end program with win/lose text after completion.
- I hope to gain enough valuable experience with Haskell that I can comfortably work with it on a regular basis
- I am very excited for this project and intend to add it to my resume

### Secondary Goals
- Using actual colors for pegs
- Inputting colored pegs using a 'currently selected tile' highlighted ui piece
- Restart and play again ui

### Stretch Goals
- Basic victory/lose screen
- Allow extra rule toggle buttons such as a duplicate color button (changes key-peg and other logic)

I aimed to be realistic with these goals and how likely I am to achieve them.

### Crude Sketch of Planned UI 
![mastermind](https://i.imgur.com/vbDcMVc.png)

# Milestone 2: Updates
### Overall progress
As seen above in my feature calendar chart, I'm progressing as expected, and am likely going to be done with the core functionality (the MVP) by the end of this week. This will give me some extra time to implement the secondary and stretch goals (2 of which I've actually finished already: colored pegs, and selected/highlighted tile). I expect to complete more than I originally intended, as this project is turning out to be even more fun than I originally anticipated.

### Current UI (Note: working directional movement and confirm button (enter))
![mastermind](https://i.imgur.com/oHQ5PtB.jpg)

### Architecture 
After studying the brick library and common implementations (including the starter code), I've split up the project into a few different components: View, Control, Model, and Main. Code for the UI implementation/manipulation, attributes, etc. are in [View.hs](https://github.com/Kyle-Stadelmann/Mastermind/blob/main/src/View.hs). All user input and helper functions that mutate the main state object, can be found in [Control.hs](https://github.com/Kyle-Stadelmann/Mastermind/blob/main/src/Control.hs). Classes/types that define the main state object (as well as the corresponding initialize state), are located in [Model.hs](https://github.com/Kyle-Stadelmann/Mastermind/blob/main/src/Model.hs). Note, there is also a Model folder with various helper functions/types that the main model component uses. And finally, the start of the program can be found in [Main.hs](https://github.com/Kyle-Stadelmann/Mastermind/blob/main/src/Main.hs).

The most noteable design pieces come from Model.hs.
```
data PlayState = PS
  { psCode   :: Board.Code   -- ^ generated answer color code
  , psTurn   :: Int         -- ^ what turn we are on (current row)
  , psBoard  :: Board.Board     -- ^ current board (player rows)
  , psPos    :: Board.Pos       -- ^ current cursor (within the current row)
  , psHints  :: Board.Hints     -- ^ current hints given thus far
  , psResult :: Maybe Board.Result    -- ^ game result
  } 
```
This defines the core state object that Control manipulates and UI displays. For a brief overview, we have the generated code that the user is trying to guess, the turn number, the board (mapping of pos -> Peg Color), pos (row,col pairs), the color hints we've given thus far, and the current result of the game (Win, Lose, Ongoing).

### Libraries
I don't plan on using many libraries other than the base ones from the starter code. Namely, Brick, Vty (for colors, attributes, etc. for the UI portion), maps (from the Data library), random (for generating a secret code), and potentially various prelude functions. 

### Challenges
So far the biggest challenges were definitely in designing all the various objects/Playstate and fighting the UI. When deciding which data types best represent the game objects, I went through each piece of the game and came up with a few different representations. Quickly, I was able to see what worked and what didn't. But I certainty found that as I moved on to actual implementations that the designs I had planned did not hold up. So I was stuck in indecisiveness switching back and forth between different models for board, pos, result, etc. before finally deciding on the ones above. 

The other big challenge (and definitely the largest time sink) was developing the UI. Firstly, I was quite disappointed at the fact that there are no default circles (and ASCII circles would be too big to fit on one screen). Also, one of my stretch goals was to allow various configurations of the game (more/less rows or columns), but after fighting to use generic padding/centering for hours, I finally decided to use constant offsets that would not work for other configurations. This means, that if I still intend to achieve that goal, I will likely have only a few different set configs instead that each have their own manual paddings (that I decide from trial and error). Overall, centering has been very annoying to deal with due to the way brick works with spacing, but I believe I've settled on something I'm happy with.
