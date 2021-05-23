(** [Command] is for parsing input to move squares *)

(** The data type that represents a square label which has one character
    between a-h and a digit between 1-8 *)
type square_label = char * int

(** The data type that represents two squares on the board: the starting
    square and the ending square in a move. *)
type squares_move = square_label * square_label

(** The data type representing a command. *)
type command =
  | Move of squares_move
  | Forfeit

(** Raised when the command is not in correct form. Correct forms are:
    move followed by two strings, undo, forfeit, hint Each of these
    commands can be separated with any number of spaces and can
    begin/end with any number of spaces also *)
exception IllegalCommand

(** Raised when the square label is not a valid square label. A valid
    square label has one character between a-h and a digit between 1-8 *)
exception IllegalSquare

(** Raised when no command is given or the command is empty. *)
exception NoCommand

(** Parses the command and returns the type of command, and if the
    command is move, it returns a type Move of [squares_move].

    If no command is given raises NoCommand If command is not in the
    correct form an IllegalCommand is raised. *)
val parse : string -> command
