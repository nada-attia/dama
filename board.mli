(* The abstract type of values representing the board. *)
type t

(** The colors of the 2 kinds of pieces *)
type color

(** A square on an n * n board *)
type square

val get_init_player : color

val get_other_player : color -> color

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

val get_square : char * int -> t -> square

(** [where_move board square] describes the squares where any given
    piece is allowed to move to, from its current position. This depends
    on it's piece type and current position, [square]. *)
val where_move : t -> square -> square list

(** [can_move square board] describes whether or not a given piece
    occupying [square] is allowed to move based off of th rules of Dama.
    true = it can and false = it cannot. Assumes: It is the move of the
    color who's piece occupies [square]*)
val can_move : square -> t -> color -> bool

(* [get_jumps sq brd clr func] describes all possible locations the
   occupant of square [square] can jump as a list of squares going in
   the direction helper function [func] describes. (Recall that possible
   jumps are required to be taken in Dama, but 2 jumps could be an
   option.) *)
val get_jumps_dir :
  square ->
  t ->
  color ->
  (square -> color -> t -> square list) ->
  square list

val update_board : bool -> t -> char * int -> char * int -> unit
