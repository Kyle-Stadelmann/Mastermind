# Mastermind
For my CSE 230 final project I intend to develop the game [Mastermind](https://en.wikipedia.org/wiki/Mastermind_(board_game)) in Haskell using the [brick library](https://github.com/jtdaugherty/brick/). 

## Quick Game Overview
Mastermind is a 2-player puzzle game where one player is the "codemaker" and the other the "codebreaker". While there are multiple variations of the rules/board, the one I will be implementing consists of 10 rows of 4 peg slots each. Pegs come in 6 different colors. The codemaker (the computer) will choose an order of 4 different colored pegs, and the player's goal is to guess this combination. The player has 10 attempts at doing so. Each attempt will result in the computer giving hints. The hints work as follows: for every correctly placed colored-peg, one red 'key-peg' is displayed, and for every other colored-peg that contains a correct color but wrong position, one white 'key-peg' is displayed. The player uses logic based on these key-pegs to make their next guess. The player wins if they guess correctly on or before their 10th attempt.

## Installation
Ensure [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install) is installed. And then enter the command: <br />
``` stack install ```

## Usage
``` stack run ```

## End Result
![Final Result](https://i.imgur.com/SlqsAhE.jpg)
