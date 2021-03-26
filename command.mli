(** The abstract data type that represents two squares on the board: the
    starting square and the ending square in a move *)
type squares_move

(** The abstract data type representing a command *)
type command

(** *)
exception IllegalCommand

(** *)
exception NoCommand

(** *)
val parse : string -> command
