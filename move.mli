type direction =
  | Up
  | Down
  | Left
  | Right

exception SquareNotFound

val get_square : char * int -> Board.t -> Board.square

val get_square_dir :
  Board.square ->
  Board.t ->
  Board.color ->
  direction ->
  Board.square list

(** [where_move board square] describes the squares where any given
    piece is allowed to move to, from its current position. This depends
    on it's piece type and current position, [square]. *)
val where_move : Board.t -> Board.square -> Board.square list

(** [can_move square board] describes whether or not a given piece
    occupying [square] is allowed to move based off of th rules of Dama.
    true = it can and false = it cannot. Assumes: It is the move of the
    color who's piece occupies [square]*)
val can_move : Board.square -> Board.t -> Board.color -> bool

val get_movable_squares_reg :
  Board.square -> Board.color -> Board.t -> Board.square list

(* [get_all_jumps sq brd clr] describes the list of squares that
   represent all possible jumps avalible for the piece of color [clrf]
   on square [sq] on board [brd] *)
val get_all_jumps :
  Board.square ->
  Board.t ->
  Board.color ->
  (Board.square * Board.square) list

val update_board :
  Board.color ->
  Board.square option ->
  Board.t ->
  char * int ->
  char * int ->
  unit

val get_all_jumps_lady :
  Board.square ->
  Board.t ->
  Board.color ->
  (Board.square * Board.square) list

val get_all_jumps_color :
  Board.square list list ->
  Board.t ->
  Board.color ->
  (Board.square * Board.square) list

val where_move_all :
  Board.square list list -> Board.t -> Board.color -> Board.square list
