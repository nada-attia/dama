type current =
  | InProgress
  | Finished
  | Pregame

type state = {
  turn : Board.color;
  board : Board.t;
  current : current;
}

let init_state board =
  { turn = Board.get_init_player; board; current = Pregame }

let get_turn state = state.turn

let update_state_move board m = failwith "TODO"

let update_state_undo board m = failwith "TODO"

let update_state_forfeit board m = failwith "TODO"

let update_state_hint board m = failwith "TODO"

let update_state (state : state) (command : Command.command) =
  init_state state.board

(* match command with | Move m -> update_state_move state m | Undo ->
   update_state_undo state | Forfeit -> update_state_forfeit state |
   Hint -> update_state_hint state *)
