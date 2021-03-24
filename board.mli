(** The abstract type of values representing the board. *)
type t

type color

type square

(** [game_init n] is a board of size n x n with all Men pieces set up in
    their correct starting positions *)
val game_init : int -> t

(** [terminal_rep_string board] is a string representation of the board
    [board]*)
val terminal_rep_string : t -> string

(** [count_piece p_color] is the total number of pieces which have color
    [p_color] and are active on the board *)
val count_active : t -> color -> int

(** [count_piece p_color] is the total number of pieces which have color
    [p_color] and are active on the board *)
val count_inactive : t -> color -> int
