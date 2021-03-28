type state

type current

val get_turn : state -> Board.color

val update_state : state -> Command.command -> state
