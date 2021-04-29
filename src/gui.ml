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
  display_image "images/info.png" 0 0;
  display_image "images/title.png" title_x 640;
  display_image "images/white-player.png" white_x 550;
  display_image "images/black-player.png" black_x 550;
  display_image "images/captured-pieces.png" white_x 450;
  display_image "images/captured-pieces.png" black_x 450

let display_board state is_ai =
  display_turn state is_ai;
  let b = State.get_board state in
  display_board_info ();
  display_sideboard b;
  let board = Board.get_board b in
  let rec print_board x y = function
    | [] -> ()
    | sq_lst :: remaining_rows ->
        let rec print_row x y = function
          | [] ->
              print_board x_margin (y + square_offset) remaining_rows
          | sq :: remaining_sqs ->
              (let piece = Board.get_occupant sq in
               match piece with
               | None -> display_image "images/empty-square.png" x y
               | Some p -> display_piece sq x y);
              print_row (x + square_offset) y remaining_sqs
        in
        print_row x y sq_lst
  in
  print_board x_margin y_margin board

let get_y_pos y =
  if y < y_margin || y > y_margin + (8 * square_offset) then -1
  else
    let y_int = ((y - y_margin) / square_offset) + 1 in
    y_int

let get_x_pos x =
  if x < x_margin || x > x_margin + (8 * square_offset) then -1
  else (x - x_margin) / square_offset

let get_x_pos_string x =
  let x_int = get_x_pos x in
  let a = Char.code 'a' in
  let c = a + x_int in
  Char.escaped (Char.chr c)

let get_board_pos x y =
  let row_pos = string_of_int (get_y_pos y) in
  let col_pos = get_x_pos_string x in
  let pos = col_pos ^ row_pos in
  if String.length pos = 2 then pos else ""

let highlight_selected x y board =
  let num_square_col = (x - x_margin) / square_offset in
  let num_square_row = (y - y_margin) / square_offset in
  let lower_x = x_margin + (num_square_col * square_offset) in
  let lower_y = y_margin + (num_square_row * square_offset) in
  let c = get_x_pos_string x in
  let num = get_y_pos y in
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

let rec display_rules state is_ai =
  display_image "images/rules.png" 0 0;
  let event = wait_next_event [ Button_down ] in
  let x = event.mouse_x in
  let y = event.mouse_y in
  if x >= 0 && x <= 80 && y >= 0 && y <= 30 then (
    Graphics.clear_graph ();
    display_board state is_ai)
  else display_rules state is_ai

let rec get_mouse_click state is_ai =
  let b = State.get_board state in
  let event = wait_next_event [ Button_down ] in
  let x = event.mouse_x in
  let y = event.mouse_y in
  if x >= 0 && x <= 50 && y >= 0 && y <= 50 then
    display_rules state is_ai
  else ();
  if get_x_pos x = -1 || get_y_pos y = -1 then
    get_mouse_click state is_ai
  else (
    highlight_selected x y b;
    get_board_pos x y)

let rec next_move state is_ai =
  display_board state is_ai;
  let player = State.player_turn state in
  let error_x = (window_width / 2) - 200 in
  let error_y = 55 in
  (if player = "black" && is_ai then (
   display_image "images/ai-thinking.png" error_x error_y;
   let new_state = Ai.ai_next_move state ai_level in
   display_image "images/clear-error.png" error_x error_y;
   next_move new_state is_ai)
  else
    let start_pos = get_mouse_click state is_ai in
    display_image "images/clear-error.png" error_x error_y;
    let end_pos = get_mouse_click state is_ai in
    let command = "move " ^ start_pos ^ " " ^ end_pos in
    print_endline command;
    match State.update_state state (Command.parse command) with
    | state ->
        display_image "images/clear-error.png" error_x error_y;
        next_move state is_ai
    | exception State.IllegalMove ->
        display_image "images/illegal-move.png" error_x error_y;
        next_move state is_ai
    | exception State.RequiredJumpNotTaken ->
        display_image "images/required-jump.png" error_x error_y;
        next_move state is_ai
    | exception Board.EmptyStartSquare ->
        display_image "images/empty-start-square.png" error_x error_y;
        next_move state is_ai
    | exception Board.NoPiece ->
        display_image "images/empty-start-square.png" error_x error_y;
        next_move state is_ai);
  ()

let play_game board is_ai =
  let state = State.init_state board in
  next_move state is_ai

let init_window =
  open_graph
    (" "
    ^ string_of_int window_width
    ^ "x"
    ^ string_of_int window_height);
  set_window_title "Dama"

let rec choose_type_game () =
  display_image "images/home.png" 0 0;
  let event = wait_next_event [ Button_down ] in
  let x = event.mouse_x in
  let y = event.mouse_y in
  if x >= 300 && x <= 800 && y > 230 && y < 310 then (
    Graphics.clear_graph ();
    let initial = Board.game_init 8 in
    play_game initial false)
  else if x >= 300 && x <= 800 && y > 350 && y < 430 then (
    Graphics.clear_graph ();
    let initial = Board.game_init 8 in
    play_game initial true)
  else choose_type_game ()

let () =
  init_window;
  choose_type_game ()
