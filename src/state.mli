type state

type current

exception IllegalMove

exception RequiredJumpNotTaken

val player_turn : state -> string

val init_state : Board.t -> state

val get_turn : state -> Board.color

(* val update_state_move : Board.t -> Command.squares_move -> _ *)
val update_state : state -> Command.command -> state

val get_board : state -> Board.t

val game_over : state -> bool
