type state

type current

(* val update_state_move : Board.t -> Command.squares_move -> _ *)

val player_turn : state -> string

exception IllegalMove

val init_state : Board.t -> state

val get_turn : state -> Board.color

val update_state : state -> Command.command -> state

val get_board : state -> Board.t

val game_over : state -> bool
