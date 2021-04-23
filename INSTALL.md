### System Requirement

OCaml 4.11.1

The game may work on other versions, but we have only tested it on this version.

### Packages needed

- ANSITerminal
- Lablgtk
- Graphics
- Camlimages

#### Package Installation Instructions

To install lablgtk on MacOS:

```
$ brew install gtk+
$ export PKG_CONFIG_PATH=/opt/X11/lib/pkgconfig
$ opam install lablgtk
```

To install Graphics and Camlimages:

`opam install graphics camlimages`

### How to run this game locally

- Clone this repository
- `cd` to the cloned folder
- run `make play` to start the game in terminal
- run `make gui` to start the GUI

### How to play the game

Once the game is started, you can move a piece by entering `move <starting position> <ending position>` where the start/end positons are strings that contain a character followed by an integer. To determine the correct starting postion and ending position of your move, use the board's grid system.

For example `move c3 c4` moves the piece located in square c3 to square c4.

To end the game, you can run `forfeit` when your player is prompted to make a move.
