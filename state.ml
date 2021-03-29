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

(* [find_square square] is true if [end_pos] is a valid ending square
   and false otherwise *)
let rec find_square square = function
  | [] -> false
  | h :: t -> if h = square then true else find_square square t

let rec find_jump square jumps =
  match jumps with
  | [] -> None
  | (captured, _) :: t ->
      if captured = square then Some square else find_jump square t

(* need to implement move for jumps *)
let update_state_move (state : state) (m : Command.squares_move) =
  let board = state.board in
  let turn = state.turn in
  let start_pos, end_pos = m in
  let square = Board.get_square end_pos board in
  let is_valid_start = Board.can_move square board turn in
  let valid_ends = Board.where_move board square in
  let is_valid_end = find_square square valid_ends in
  let jumps = Board.get_all_jumps square board turn in
  let captured = find_jump square jumps in
  if is_valid_start = true && is_valid_end = true then (
    Board.update_board turn captured board start_pos end_pos;
    let new_turn = Board.get_other_player turn in
    let new_current =
      if
        Board.count_inactive board turn <> 0
        && Board.count_inactive board new_turn <> 0
      then InProgress
      else Finished
    in
    { turn = new_turn; board; current = new_current })
  else raise IllegalMove

let update_state_undo state = failwith "Unimplimented"

let update_state_forfeit state =
  let new_turn = Board.get_other_player state.turn in
  { turn = new_turn; board = state.board; current = Finished }

let update_state_hint state = failwith "Unimplimented"

let update_state (state : state) (command : Command.command) =
  match command with
  | Move m -> update_state_move state m
  | Undo -> update_state_undo state
  | Forfeit -> update_state_forfeit state
  | Hint -> update_state_hint state
