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

let get_board state = state.board

let init_state board =
  { turn = Board.get_init_player; board; current = Pregame }

let get_turn state = state.turn

let player_turn state =
  match get_turn state with Black -> "black" | White -> "white"

(* [find_square square] is true if [end_pos] is a valid ending square
   and false otherwise *)
let rec find_square square = function
  | [] -> false
  | h :: t -> if h = square then true else find_square square t

let rec find_jump end_square = function
  | [] -> None
  | ((captured : Board.square), (final : Board.square)) :: t ->
      if final = end_square then Some captured
      else find_jump end_square t

let find_final_jump = function
  | [] -> failwith "No jumps left"
  | (_, (final : Board.square)) :: t -> Board.get_label final

let rec make_all_jumps start_pos end_pos board turn =
  let square_start = Move.get_square start_pos board in
  let square_end = Move.get_square end_pos board in
  let _, role = Board.get_piece_info square_start in
  let jumps =
    if role = Board.Man then Move.get_all_jumps square_start board turn
    else Move.get_all_jumps_lady square_start board turn
  in
  let captured = find_jump square_end jumps in
  Move.update_board turn captured board start_pos end_pos;
  if jumps <> [] then
    try
      let new_jumps = Move.get_all_jumps square_end board turn in
      let new_end = find_final_jump new_jumps in
      make_all_jumps end_pos new_end board turn
    with _ -> ()
  else ()

(* need to implement move for jumps *)
let update_state_move (state : state) (m : Command.squares_move) =
  let board = state.board in
  let turn = state.turn in
  let start_pos, end_pos = m in
  let square_start = Move.get_square start_pos board in
  let square_end = Move.get_square end_pos board in
  let is_valid_start = Move.can_move square_start board turn in
  let valid_ends = Move.where_move board square_start in
  let is_valid_end = find_square square_end valid_ends in
  if is_valid_start && is_valid_end then (
    make_all_jumps start_pos end_pos board turn;
    let new_turn = Board.get_other_player turn in
    let new_current =
      if
        Board.count_inactive board new_turn = 16
        || Move.can_move_all board new_turn = false
      then Finished
      else InProgress
    in
    (* If the game is over, we want to change the turn to the color that
       won so we can print it in terminal*)
    let new_turn = if new_current = Finished then turn else new_turn in
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

let game_over state =
  match state.current with
  | Pregame -> false
  | InProgress -> false
  | Finished -> true
