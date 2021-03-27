(** The abstract type of values representing the board. *)
type t

(** The colors of the 2 kinds of pieces *)
type color

(** A square on an n * n board *)
type square

(** [game_init n] is a board of size n x n with all Men pieces set up in
    their correct starting positions. Requires: n is between 8 and 26
    inclusive*)
val game_init : int -> t

(** [terminal_rep_string board count] is a string representation of the
    board [board] with row and column labels starting at [count]. For
    example, [terminal_rep_string board 1] will have labels for the rows
    from 1 to size of the board and column labels starting at 'A'.
    Requires: count > 0 *)
val terminal_rep_string : t -> int -> string

(** [count_inactive p_color] is the total number of pieces which have
    color [p_color] and are not on the board *)
val count_inactive : t -> color -> int
