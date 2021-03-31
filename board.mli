(* The abstract type of values representing the board. *)
type t

type color =
  | Black
  | White

(** The colors of the 2 kinds of pieces *)

type role =
  | Man
  | Lady

type direction =
  | Up
  | Down
  | Left
  | Right

exception NoPiece

exception SquareNotFound

(** A square on an n * n board *)

val get_init_player : color

val get_other_player : color -> color

(** [game_init n] is a board of size n x n with all Men pieces set up in
    their correct starting positions. Requires: n is between 8 and 26
    inclusive*)
val game_init : int -> t

val get_board : t -> square list list

(** [terminal_rep_string board count] is a string representation of the
    board [board] with row and column labels starting at [count]. For
    example, [terminal_rep_string board 1] will have labels for the rows
    from 1 to size of the board and column labels starting at 'A'.
    Requires: count > 0 *)
val terminal_rep_string : t -> int -> string

(** [count_inactive p_color] is the total number of pieces which have
    color [p_color] and are not on the board *)
val count_inactive : t -> color -> int

val get_label : square -> char * int

val get_square : char * int -> t -> square

val get_square_dir : square -> t -> color -> direction -> square list

val get_sqlst_label : square list -> (char * int) list

(** [where_move board square] describes the squares where any given
    piece is allowed to move to, from its current position. This depends
    on it's piece type and current position, [square]. *)
val where_move : t -> square -> square list

(** [can_move square board] describes whether or not a given piece
    occupying [square] is allowed to move based off of th rules of Dama.
    true = it can and false = it cannot. Assumes: It is the move of the
    color who's piece occupies [square]*)
val can_move : square -> t -> color -> bool

(* [get_all_jumps sq brd clr] describes the list of squares that
   represent all possible jumps avalible for the piece of color [clrf]
   on square [sq] on board [brd] *)
val get_all_jumps : square -> t -> color -> (square * square) list

val update_board :
  color -> square option -> t -> char * int -> char * int -> unit

val get_piece_info : square -> color * role

val get_movable_squares_reg : square -> color -> t -> square list
