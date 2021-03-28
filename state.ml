type current =
  | InProgress
  | Finished
  | Pregame

type state = {
  turn : Board.color;
  board : Board.t;
  current : current;
}

exception IllegalMove

let init_state board =
  { turn = Board.get_init_player; board; current = Pregame }

let get_turn state = state.turn

let update_state_move (state : state) (m : Command.squares_move) =
  let board = state.board in
  let turn = state.turn in
  let start_pos, end_pos = m in
  let square = Board.get_square end_pos board in
  let valid_move = Board.can_move square board turn in
  if valid_move = true then (
    Board.update_board board start_pos end_pos;
    init_state board)
  else raise IllegalMove

let update_state_undo state = failwith "TODO"

let update_state_forfeit state = failwith "TODO"

let update_state_hint state = failwith "TODO"

let update_state (state : state) (command : Command.command) =
  match command with
  | Move m -> update_state_move state m
  | Undo -> update_state_undo state
  | Forfeit -> update_state_forfeit state
  | Hint -> update_state_hint state
