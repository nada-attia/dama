(** The abstract type of values representing the board. *)
type t

(** The colors of the 2 kinds of pieces *)
type color

(** A square on an n * n board *)
type square

(** [game_init n] is a board of size n x n with all Men pieces set up (*
    in their correct starting positions. Requires: n is between 8 and 26
    inclusive*) val board_init : int -> t *)

(* (** [terminal_rep_string board] is a string representation of the
   board [board]*) val terminal_rep_string : t -> string

   (** [count_inactive p_color] is the total number of pieces which have
   color [p_color] and are not on the board *) val count_inactive : t ->
   color -> int *)
