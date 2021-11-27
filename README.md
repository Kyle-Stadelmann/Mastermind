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
| Helper functions to manipulate UI (state) using data structures            | 11/22/21      |
| Helper functions to take input and display on board                        | 11/24/21      |
| Game logic functions: generate code + generate key pegs based off input    | 11/27/21      |     ✔
| Game logic functions: increment round                                      | 12/1/21       |
| Wrap up: confirm easy installation on other environments, unit tests, etc. | 12/4/21       |
| Stretch/secondary goals: win/lose screens, restart buttons, etc.           | 12/7/21       |
| Prepare for presentation                                                   | 12/9/21       |

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
