(** The abstract data type representing the state of the game *)
type state

(** Raised when the player tries to make an illegal move*)
exception IllegalMove

(** Raised when the player tries to make a move when one of their pieces
    has a jump available *)
exception RequiredJumpNotTaken

(** [init_state board] is the initial state when the game first starts *)
val init_state : Board.t -> state

(** [player_turn state] is the string representation of the player *)
val player_turn : state -> string

(** [get_turn state] is the color of the player whose turn it is *)
val get_turn : state -> Board.color

(** [update_state state command] is the new state after the command is
    executed. Raises an exception if the command is not a valid one *)
val update_state : state -> Command.command -> bool -> state

(** [get_board state] is the current board at a given state of the game *)
val get_board : state -> Board.t

(** [game_over state] is true if the game is over and false otherwise *)
val game_over : state -> bool

(** [copy_state state] creates a copy of the state which is used in the
    Monte Carlo Tree Search AI *)
val copy_state : state -> state
