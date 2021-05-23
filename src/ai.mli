(** [Ai] implements the logic for the AI*)

(** [ai_next_move st depth] is the state of the game after the ai takes
    a move from [st] with [depth] for the tree search algorithm that
    determines the ai's next move*)
val ai_next_move : Board.color -> State.state -> int -> State.state
