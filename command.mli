(** The abstract data type that represents two squares on the board: the
    starting square and the ending square in a move *)
type squares_move = string * string

(** The abstract data type representing a command *)
type command =
  | Move of squares_move
  | Undo
  | Forfeit
  | Hint

(** *)
exception IllegalCommand

(** *)
exception NoCommand

(** *)
val parse : string -> command
