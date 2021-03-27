(* type state

   val update_state : Board.t -> Command.command -> state

   (** [where_move board square] describes the squares where any given
   piece is allowed to move to, from its current position. This depends
   on it's piece type and current position, [square]. *) val where_move
   : Board.t -> Board.square -> Board.square list

   (** [can_move square board] describes whether or not a given piece
   occupying [square] is allowed to move based off of th rules of Dama.
   true = it can and false = it cannot. Assumes: It is the move of the
   color who's piece occupies [square]*) val can_move : Board.square ->
   Board.color -> Board.t -> bool

   (** [can_jump board color] describes whether or not a given piece on
   the board can perform a jump move. *) val can_jump : Board.t ->
   Board.color -> bool *)
