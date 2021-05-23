(** [Move] contains functions for moving pieces on the board*)

(** the type representing the 4 possible directions a piece can move.*)
type direction =
  | Up
  | Down
  | Left
  | Right

(** Raised when a specified square is not found on the current board.*)
exception SquareNotFound

(** [get_square label board] is the square at position [label] on board
    [board]. Raises an exception when a square is not found.*)
val get_square : char * int -> Board.t -> Board.square

(** [get_square_dir square board color direction] is the square in
    direction [direction] from square [square] for a piece of color
    [color] on board [board]. Raises an exception when a square is not
    found .*)
val get_square_dir :
  Board.square ->
  Board.t ->
  Board.color ->
  direction ->
  Board.square list

(** [where_move board square] are the squares a piece on square [square]
    in board [board] it is allowed to move to. Raises an exception if
    the provided square is empty. *)
val where_move : Board.t -> Board.square -> Board.square list

(** [where_move_all board color] is all of the squares that can be moved
    to for the curently active pieces of color [color] on board [board]. *)
val where_move_all :
  Board.t -> Board.color -> (Board.square * Board.square list) list

(** [can_move square board color] is true if a piece of color [color] on
    square [square] in board [board] is currently able to move, and
    false otherwise. *)
val can_move : Board.square -> Board.t -> Board.color -> bool

(** [get_movable_squares_reg square color board] is all of the squares a
    regular piece of color [color] on square [square] in board [board]
    can move to. *)
val get_movable_squares_reg :
  Board.square -> Board.color -> Board.t -> Board.square list

(** [get_all_jumps square board color] is all of the (jumped square,
    destination square)'s that a regular piece of color [color] on
    square [square] in board [board] is avalible to take and arrive at. *)
val get_all_jumps :
  Board.square ->
  Board.t ->
  Board.color ->
  (Board.square * Board.square) list

(** [update_board color square board label1 label2] updates the board
    for a player of color [color] after the player has captured a piece
    on square [square] by moving from [label1] to [label2] in board
    [board]. *)
val update_board :
  Board.color ->
  Board.square option ->
  Board.t ->
  char * int ->
  char * int ->
  unit

(** [get_all_jumps_lady square board color] is all of the (jumped
    square, destination square)'s that a lady piece of color [color] on
    square [square] in board [board] is avalible to take and arrive at. *)
val get_all_jumps_lady :
  Board.square ->
  Board.t ->
  Board.color ->
  (Board.square * Board.square) list

(** [can_move_all board color] is true if any piece of color [color] in
    board [board] is currently able to move, and false otherwise. *)
val can_move_all : Board.t -> Board.color -> bool

(** [exists_jumps color board] is true if any valid jumps exist for a
    piece of color [color] in board [board], and false otherwise. *)
val exists_jumps : Board.color -> Board.t -> Board.square list

(** [get_where_jump color board] is all of the destination squares for
    all of jumps avalible for pieces of color [color] in board [board]. *)
val get_where_jump : Board.color -> Board.t -> Board.square list
