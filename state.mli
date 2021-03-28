type state

type current

exception IllegalMove

val init_state : Board.t -> state

val get_turn : state -> Board.color

val update_state : state -> Command.command -> state
