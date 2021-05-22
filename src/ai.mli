(** [ai_next_move st depth] is the state of the game after the ai takes
    a move from [st] with [depth] for the tree search algorithm that
    determines the ai's next move*)
val ai_next_move : State.state -> int -> State.state
