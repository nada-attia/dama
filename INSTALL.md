### System Requirement

OCaml 4.11.1

The game may work on other versions, but we have only tested it on this version.

### Packages needed

- ANSITerminal
- Lablgtk
- Graphics
- Camlimages

#### Package Installation Instructions

To install lablgtk, graphics and camlimages on MacOS:

```
brew install gtk+
export PKG_CONFIG_PATH=/opt/X11/lib/pkgconfig
opam install lablgtk
opam install graphics camlimages
```

To install lablgtk, graphics and camlimages on Windows:

- Create a new switch using the instructions on the [3110 Canvas site](https://canvas.cornell.edu/courses/25259/pages/create-an-opam-switch)
- When you reach the `opam install...` part of the instructions, replace that command with

```
opam install -y utop ounit qcheck ocaml-lsp-server ocamlformat yojson ansiterminal csv bisect_ppx-ocamlbuild menhir graphics lablgtk camlimages user-setup
```

### How to run this game locally

- Clone this repository
- `cd` to the cloned folder
- run `make play` to start the game in terminal
- run `make gui` to start the GUI

#### To run the GUI on Windows:

- Install [Xming](https://sourceforge.net/projects/xming/files/Xming/), [Xming fonts](https://sourceforge.net/projects/xming/files/Xming-fonts/7.7.0.10/), and [PuTTY](https://www.putty.org/)
- Follow the instructions on this [website](https://aruljohn.com/info/x11forwarding/).
- Instead of typing "textbox" as host name (as detailed in the above instructions), run `nslookup <ip address>` where `<ip address>` is your ip address. You can find your ip address by googling "what is my ip address?"
- Copy the text that follows "name:" and paste it into the host name field (instead of "textbox").
- Open the session in PuTTY, and you should be able to run the gui by running `make gui` in terminal.

### How to play the game

Once the game is started, you can move a piece by entering `move <starting position> <ending position>` where the start/end positons are strings that contain a character followed by an integer. To determine the correct starting postion and ending position of your move, use the board's grid system.

For example `move c3 c4` moves the piece located in square c3 to square c4.

To end the game, you can run `forfeit` when your player is prompted to make a move.
