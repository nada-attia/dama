type state

val update_state : Board.t -> Command.command -> state

val where_move : Board.t -> Board.square -> Board.square list

val can_move : Board.t -> Board.color -> bool

val can_jump : Board.t -> Board.color -> bool
