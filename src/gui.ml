open Graphics
open Graphic_image
open Png
open Constants

let display_image img_path x y =
  let img = Png.load_as_rgb24 img_path [] in
  let graphics_img = Graphic_image.of_image img in
  Graphics.draw_image graphics_img x y

let display_piece sq x y =
  let c, r = Board.get_piece_info sq in
  if c = Board.Black && r = Board.Man then
    display_image "images/black-w-background.png" x y
  else if c = Board.Black && r = Board.Lady then
    display_image "images/black-lady.png" x y
  else if c = Board.White && r = Board.Lady then
    display_image "images/white-lady.png" x y
  else display_image "images/white-w-background.png" x y

let display_sideboard board =
  let white_x = (board_size / 2) - 200 in
  let black_x = window_width - (board_size / 2) in
  let lady_y = 380 in
  let man_y = 320 in
  let w_lady_count, w_man_count =
    Board.get_side_board board Board.White
  in
  let b_lady_count, b_man_count =
    Board.get_side_board board Board.Black
  in
  display_image "images/white-lady-clear.png" black_x lady_y;
  display_image
    ("images/numbers/" ^ string_of_int w_lady_count ^ ".png")
    (black_x + 50) lady_y;
  display_image "images/black-lady-clear.png" white_x lady_y;
  display_image
    ("images/numbers/" ^ string_of_int b_lady_count ^ ".png")
    (white_x + 50) lady_y;
  display_image "images/white.png" black_x man_y;
  display_image
    ("images/numbers/" ^ string_of_int w_man_count ^ ".png")
    (black_x + 50) man_y;
  display_image "images/black.png" white_x man_y;
  display_image
    ("images/numbers/" ^ string_of_int b_man_count ^ ".png")
    (white_x + 50) man_y

let display_turn state is_ai =
  let player = State.player_turn state in
  let white_x = (board_size / 2) - 200 in
  let black_x = window_width - (board_size / 2) in
  let turn_y = 520 in
  if player = "white" then (
    display_image "images/clear-turn.png" black_x turn_y;
    display_image "images/your-turn.png" white_x turn_y)
  else (
    display_image "images/clear-turn.png" white_x turn_y;
    let img =
      if is_ai then "images/ai-turn.png" else "images/your-turn.png"
    in
    display_image img black_x turn_y)

let display_board_info () =
  let title_x = (window_width / 2) - 100 in
  let white_x = (board_size / 2) - 200 in
  let black_x = window_width - (board_size / 2) in
  display_image "images/hint.png" 870 20;
  display_image "images/forfeit.png" 980 20;
  display_image "images/info.png" 0 0;
  display_image "images/title.png" title_x 640;
  display_image "images/white-player.png" white_x 550;
  display_image "images/black-player.png" black_x 550;
  display_image "images/captured-pieces.png" white_x 450;
  display_image "images/captured-pieces.png" black_x 450

let get_y_pos state y =
  if y < y_margin || y > y_margin + (8 * square_offset) then -1
  else
    let y_int = ((y - y_margin) / square_offset) + 1 in
    if State.get_turn state = Board.Black then 9 - y_int else y_int

let get_x_pos x =
  if x < x_margin || x > x_margin + (8 * square_offset) then -1
  else (x - x_margin) / square_offset

let get_x_pos_string x =
  let x_int = get_x_pos x in
  let a = Char.code 'a' in
  let c = a + x_int in
  Char.escaped (Char.chr c)

let get_board_pos state x y =
  let row_pos = string_of_int (get_y_pos state y) in
  let col_pos = get_x_pos_string x in
  let pos = col_pos ^ row_pos in
  if String.length pos = 2 then pos else ""

let highlight_jump state board =
  let player = State.get_turn state in
  let all_jumps = Move.get_where_jump player board in
  let rec highlight_all_jumps = function
    | [] -> ()
    | sq :: remaining_sqs ->
        let c, i = Board.get_label sq in
        let num_c = Char.code c - Char.code 'a' in
        let lower_x = x_margin + (num_c * square_offset) in
        let lower_y =
          if State.get_turn state = Board.Black then
            y_margin + ((8 - i) * square_offset)
          else y_margin + ((i - 1) * square_offset)
        in
        display_image "images/can-jump.png" lower_x lower_y;
        highlight_all_jumps remaining_sqs
  in
  highlight_all_jumps all_jumps

let highlight_selected state x y board =
  let num_square_col = (x - x_margin) / square_offset in
  let num_square_row = (y - y_margin) / square_offset in
  let lower_x = x_margin + (num_square_col * square_offset) in
  let lower_y = y_margin + (num_square_row * square_offset) in
  let c = get_x_pos_string x in
  let num = get_y_pos state y in
  let square = Move.get_square (c.[0], num) board in
  match Board.get_occupant square with
  | None -> display_image "images/selected-empty.png" lower_x lower_y
  | Some p ->
      let c, r = Board.get_piece_info square in
      let image =
        if c = Board.White && r = Board.Man then
          "images/selected-white.png"
        else if c = Board.Black && r = Board.Man then
          "images/selected-black.png"
        else if c = Board.Black && r = Board.Lady then
          "images/selected-black-lady.png"
        else "images/selected-white-lady.png"
      in
      display_image image lower_x lower_y

let rec print_board x y = function
  | [] -> ()
  | sq_lst :: remaining_rows ->
      let rec print_row x y = function
        | [] -> print_board x_margin (y + square_offset) remaining_rows
        | sq :: remaining_sqs ->
            (let piece = Board.get_occupant sq in
             match piece with
             | None -> display_image "images/empty-square.png" x y
             | Some p -> display_piece sq x y);
            print_row (x + square_offset) y remaining_sqs
      in
      print_row x y sq_lst

let display_board state is_ai =
  display_turn state is_ai;
  let b = State.get_board state in
  display_board_info ();
  display_sideboard b;
  let board = Board.get_board b in
  let displayed_board =
    if State.get_turn state = Board.Black then List.rev board else board
  in
  print_board x_margin y_margin displayed_board;
  highlight_jump state b;
  if is_ai && State.get_turn state = Board.Black then
    display_image "images/hide-hint.png" 870 20
  else ()

let rec display_rules state is_ai =
  display_image "images/rules.png" 0 0;
  let event = wait_next_event [ Button_down ] in
  let x = event.mouse_x in
  let y = event.mouse_y in
  if x >= 0 && x <= 80 && y >= 0 && y <= 30 then (
    Graphics.clear_graph ();
    display_board state is_ai)
  else display_rules state is_ai

let check_end_game state =
  let player = State.get_turn state in
  if State.game_over state then (
    Sys.remove "game.json";
    if player = Board.Black then
      display_image "images/pages/black-wins.png" 0 0
    else display_image "images/pages/white-wins.png" 0 0)
  else ()

let is_end_game state = if State.game_over state then true else false

let get_label x y state =
  let c = (get_x_pos_string x).[0] in
  let i = get_y_pos state y in
  (c, i)

let rec get_mouse_click state is_ai start =
  let b = State.get_board state in
  let event = wait_next_event [ Button_down ] in
  let x = event.mouse_x in
  let y = event.mouse_y in
  listen_for_button_clicks x y state is_ai;
  if get_x_pos x = -1 || get_y_pos state y = -1 then
    get_mouse_click state is_ai start
  else if start = true then (
    let sq = Move.get_square (get_label x y state) b in
    match Board.get_occupant sq with
    | None -> get_mouse_click state is_ai start
    | Some p ->
        highlight_selected state x y b;
        get_board_pos state x y)
  else (
    highlight_selected state x y b;
    get_board_pos state x y)

and listen_for_button_clicks x y state is_ai =
  if x >= 0 && x <= 50 && y >= 0 && y <= 50 then
    display_rules state is_ai
  else if x >= 870 && x <= 970 && y >= 20 && y <= 80 then
    if (is_ai && State.get_turn state = Board.White) || is_ai = false
    then get_hint state is_ai
    else ()
  else if x >= 980 && x <= 1080 && y >= 20 && y <= 80 then
    forfeit_game state is_ai
  else ()

and forfeit_game state is_ai =
  match State.update_state state (Command.parse "forfeit") true with
  | state -> next_move state is_ai
  | exception _ -> failwith "F"

and get_hint state is_ai =
  display_image "images/ai-move.png" error_x error_y;
  let color = State.get_turn state in
  let ai_state = Ai.ai_next_move color state ai_level in
  display_image "images/clear-error.png" error_x error_y;
  next_move ai_state is_ai

and next_move state is_ai =
  check_end_game state;
  if is_end_game state = false then (
    Yojson.Basic.to_file "game.json" (State.state_to_json state);
    display_board state is_ai;
    let color = State.get_turn state in
    let player = State.player_turn state in

    if player = "black" && is_ai then (
      display_image "images/ai-thinking.png" error_x error_y;
      let new_state = Ai.ai_next_move color state ai_level in
      display_image "images/clear-error.png" error_x error_y;
      next_move new_state is_ai)
    else
      let start_pos = get_mouse_click state is_ai true in
      display_image "images/clear-error.png" error_x error_y;
      let end_pos = get_mouse_click state is_ai false in
      let command = "move " ^ start_pos ^ " " ^ end_pos in
      print_endline command;
      display_errors state command is_ai)
  else ()

and display_errors state command is_ai =
  match State.update_state state (Command.parse command) true with
  | state ->
      display_image "images/clear-error.png" error_x error_y;
      next_move state is_ai
  | exception State.IllegalMove ->
      display_image "images/illegal-move.png" error_x error_y;
      next_move state is_ai
  | exception State.RequiredJumpNotTaken ->
      display_image "images/required-jump.png" error_x error_y;
      next_move state is_ai
  | exception Board.NoPiece ->
      display_image "images/empty-start-square.png" error_x error_y;
      next_move state is_ai

let play_game state is_ai =
  Graphics.clear_graph ();
  next_move state is_ai

let init_window =
  open_graph
    (" "
    ^ string_of_int window_width
    ^ "x"
    ^ string_of_int window_height);
  set_window_title "Dama"

let rec choose_type_game () =
  if Sys.file_exists "game.json" then (
    display_image "images/home.png" 0 0;
    display_image "images/continue-old-game.png" 335 120)
  else display_image "images/home.png" 0 0;
  let event = wait_next_event [ Button_down ] in
  let x = event.mouse_x in
  let y = event.mouse_y in
  let init_board = Board.game_init 8 in
  let init_state = State.init_state init_board in
  if x >= 300 && x <= 800 && y > 230 && y < 310 then
    play_game init_state false
  else if x >= 300 && x <= 800 && y > 350 && y < 430 then
    play_game init_state true
  else if x >= 350 && x <= 750 && y >= 140 && y <= 190 then
    let saved_game = Yojson.Basic.from_file "game.json" in
    let s = State.json_to_state saved_game in
    play_game s false
  else choose_type_game ()

let () =
  init_window;
  choose_type_game ()
