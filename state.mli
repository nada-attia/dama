type state

type current

(* val update_state_move : Board.t -> Command.squares_move -> _ *)

val init_state : Board.t -> state

val player_turn : state -> string
