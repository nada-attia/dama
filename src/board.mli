(* The abstract type of values representing the board. *)
type t

type color =
  | Black
  | White

type square

type piece

type role =
  | Man
  | Lady

exception NoPiece

exception SquareNotFound

exception EmptyStartSquare

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

val get_occupant : square -> piece option

val get_color : piece -> color

val get_sqlst_label : square list -> (char * int) list

val get_piece_info : square -> color * role

val update_piece : square -> piece option -> unit

val remove_pieces : square -> color -> t -> unit

val get_can_jump : square -> bool

val try_upgrade_piece : square -> t -> color -> unit

val update_can_jump : piece -> bool -> unit

val copy_board : t -> t
