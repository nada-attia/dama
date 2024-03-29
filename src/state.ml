open Yojson.Basic.Util

type current =
  | InProgress
  | Finished

type state = {
  turn : Board.color;
  board : Board.t;
  current : current;
}

exception IllegalMove

exception RequiredJumpNotTaken

let get_board state = state.board

let copy_state (s : state) =
  let copy_board = Board.copy_board (get_board s) in
  { s with board = copy_board }

let init_state board =
  { turn = Board.get_init_player; board; current = InProgress }

let get_turn state = state.turn

let player_turn state =
  match get_turn state with Black -> "black" | White -> "white"

let current_string state =
  match state.current with
  | InProgress -> "in-progress"
  | Finished -> "finished"

let state_to_json state : Yojson.Basic.t =
  let turn_str = player_turn state in
  let current_str = current_string state in
  let board_json = Board.board_to_json state.board in
  let sideboard_json = Board.sideboard_to_json state.board in
  `Assoc
    [
      ("board", board_json);
      ("turn", `String turn_str);
      ("current", `String current_str);
      ("sideboard", sideboard_json);
    ]

let json_to_state json =
  let turn_str = json |> member "turn" |> to_string in
  let turn = if turn_str = "black" then Board.Black else Board.White in
  let current_str = json |> member "current" |> to_string in
  let current =
    if current_str = "in-progress" then InProgress else Finished
  in
  let board_json = json |> member "board" in
  let sideboard_json = json |> member "sideboard" in
  let board = Board.t_of_board_json board_json sideboard_json in
  { turn; board; current }

(** [find_square square] is true if [end_pos] is a valid ending square
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

let check_for_required_jumps color board sq_start sq_end valid_ends =
  let c, _ = Board.get_piece_info sq_start in
  let jumps = Move.exists_jumps color board in
  if jumps <> [] && List.mem sq_end valid_ends = false && c = color then
    raise RequiredJumpNotTaken
  else ()

let update_state_move
    (state : state)
    (m : Command.squares_move)
    (check : bool) =
  let board = state.board in
  let turn = state.turn in
  let start_pos, end_pos = m in
  let square_start = Move.get_square start_pos board in
  let square_end = Move.get_square end_pos board in
  let valid_ends =
    if check then Move.where_move board square_start else []
  in
  if check then
    check_for_required_jumps turn board square_start square_end
      valid_ends
  else ();
  let is_valid_start =
    if check then Move.can_move square_start board turn else true
  in
  let is_valid_end =
    if check then find_square square_end valid_ends else true
  in
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
    Yojson.Basic.to_file "game.json"
      (state_to_json { turn = new_turn; board; current = new_current });
    { turn = new_turn; board; current = new_current })
  else raise IllegalMove

let update_state_forfeit state =
  let new_turn = Board.get_other_player state.turn in
  { turn = new_turn; board = state.board; current = Finished }

let update_state
    (state : state)
    (command : Command.command)
    (check : bool) =
  match command with
  | Move m -> update_state_move state m check
  | Forfeit -> update_state_forfeit state

let game_over state =
  match state.current with InProgress -> false | Finished -> true
