### System Requirement

OCaml 4.11.1

The game may work on other versions, but we have only tested it on this version.

### How to run this game locally

- Clone this repository
- `cd` to the cloned folder and run `make play` to start the game. There are no new OPAM packages needed. You can run this in the same OPAM switch used in CS 3110.

### How to play the game

Once the game is started, you can move a piece by entering `move <starting position> <ending position>` where the start/end positons are strings that contain a character followed by an integer. To determine the correct starting postion and ending position of your move, use the board's grid system.

For example `move c3 c4` moves the piece located in square c3 to square c4.

To end the game, you can run `forfeit` when your player is prompted to make a move.
