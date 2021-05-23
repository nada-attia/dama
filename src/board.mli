(** [t] is the abstract type of values representing the board along with
    any necessary information accociated with the board. *)
type t

(** [color] is the color of the players*)
type color =
  | Black
  | White

(** [square] is a square on the board *)
type square

(** [piece] is the abstact type representing movable pieces on the board*)
type piece

(** [role] is Man for a Man piece and Lady for a Dama piece, per rules
    of the game*)
type role =
  | Man
  | Lady

(** [NoPiece] is raised when attempting to get a piece from a square
    with no pieces on it*)
exception NoPiece

(** [EmptyStartSquare] is raised when attempting to move a piece from a
    square that has no pieces.*)
exception EmptyStartSquare

(** [get_init_player] is the color that starts the game*)
val get_init_player : color

(** [get_other_player color] is the other color that is not [color]*)
val get_other_player : color -> color

(** [game_init n] is a board of size n x n with all Men pieces set up in
    their correct starting positions. Requires: n is between 8 and 26
    inclusive*)
val game_init : int -> t

(** [get board t] is the board in [t]*)
val get_board : t -> square list list

(** [terminal_rep_string board count] is a string representation of the
    board [board] with row and column labels starting at [count]. For
    example, [terminal_rep_string board 1] will have labels for the rows
    from 1 to size of the board and column labels starting at 'A'.
    Requires: count > 0 *)
val terminal_rep_string : t -> int -> string

(** [terminal_rep_string_aux board count] is
    [terminal_rep_string board count] without labels for the columns.
    Used for testing.*)
val terminal_rep_string_aux : square list list -> int -> string

(** [count_inactive p_color] is the total number of pieces which have
    color [p_color] and are not on the board *)
val count_inactive : t -> color -> int

(** [get_label sq] is the label of [sq]*)
val get_label : square -> char * int

(** [get_occupant square] is the piece currently on [square]*)
val get_occupant : square -> piece option

(** [get_color piece] is the color of [piece]*)
val get_color : piece -> color

(** [get_sqlst_label lst] is the list of labels of squares in [lst]*)
val get_sqlst_label : square list -> (char * int) list

(** [get_piece_info sq] is the color and role of the piece on [sq]*)
val get_piece_info : square -> color * role

(** [update_piece sq piece_opt] is [sq] updated with [piece_opt]*)
val update_piece : square -> piece option -> unit

(** [remove_pieces sq t] removes the captured piece from captured square
    [sq] and updates the sideboard.*)
val remove_pieces : square -> t -> unit

(** [get_can_jump sq] is the can_jump field for the piece on [sq].
    Raises: [NoPiece exception] if there is no piece on the square.*)
val get_can_jump : square -> bool

(** [try_upgrade_piece sq t color] upgrades the piece of [color] on [sq]
    in [t] to a Lady piece if it can. Raises:
    [EmptyStartSquare exception] if there is no piece on the square *)
val try_upgrade_piece : square -> t -> color -> unit

(** [update_can_jump piece b] changes the can_jump field of [piece] to
    [b]*)
val update_can_jump : piece -> bool -> unit

(** [copy_board t] is a copy of [t]. This is useful for making a copy
    before mutating [t] *)
val copy_board : t -> t

(** [get_side_board t color] is the sideboard of [t] for [color]*)
val get_side_board : t -> color -> int * int
