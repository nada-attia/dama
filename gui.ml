open Graphics
open Graphic_image
open Png
open Constants

let display_image img_path x y =
  let img = Png.load_as_rgb24 img_path [] in
  let graphics_img = Graphic_image.of_image img in
  Graphics.draw_image graphics_img x y

let display_board board =
  display_image "images/title.png" 400 600;
  let b = Board.get_board board in
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
               | Some p ->
                   let c, r = Board.get_piece_info sq in
                   if c = Board.Black && r = Board.Man then
                     display_image "images/black-w-background.png" x y
                   else if c = Board.Black && r = Board.Lady then
                     display_image "images/black-lady.png" x y
                   else if c = Board.White && r = Board.Lady then
                     display_image "images/white-lady.png" x y
                   else
                     display_image "images/white-w-background.png" x y);
              print_row (x + square_offset) y remaining_sqs
        in
        print_row x y sq_lst
  in
  print_board x_margin y_margin b

let get_y_pos y =
  if y < y_margin || y > y_margin + (8 * square_offset) then ""
  else
    let y_int = ((y - y_margin) / square_offset) + 1 in
    string_of_int y_int

let get_x_pos x =
  if x < x_margin || x > x_margin + (8 * square_offset) then ""
  else
    let x_int = (x - x_margin) / square_offset in
    let a = Char.code 'a' in
    let c = a + x_int in
    Char.escaped (Char.chr c)

let get_board_pos x y =
  let row_pos = get_y_pos y in
  let col_pos = get_x_pos x in
  let pos = col_pos ^ row_pos in
  if String.length pos = 2 then pos else ""

let display_selected x y board =
  let num_square_col = (x - x_margin) / square_offset in
  let num_square_row = (y - y_margin) / square_offset in
  let lower_x = x_margin + (num_square_col * square_offset) in
  let lower_y = y_margin + (num_square_row * square_offset) in
  let c = (get_x_pos x).[0] in
  let num = int_of_string (get_y_pos y) in
  let square = Move.get_square (c, num) board in
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

let get_mouse_click board =
  let event = wait_next_event [ Button_down ] in
  let x = event.mouse_x in
  let y = event.mouse_y in
  display_selected x y board;
  get_board_pos x y

let rec next_move state =
  (let b = State.get_board state in
   display_board b;
   let start_pos = get_mouse_click b in
   let end_pos = get_mouse_click b in
   let command = "move " ^ start_pos ^ " " ^ end_pos in
   print_endline command;
   match State.update_state state (Command.parse command) with
   | state ->
       display_image "images/clear-error.png" 300 45;
       next_move state
   | exception Command.IllegalCommand ->
       ();
       next_move state
   | exception Command.IllegalSquare ->
       ();
       next_move state
   | exception Command.NoCommand ->
       ();
       next_move state
   | exception State.IllegalMove ->
       display_image "images/illegal-move.png" 300 45;
       next_move state
   | exception Board.EmptyStartSquare ->
       ();
       next_move state
   | exception Board.NoPiece ->
       ();
       next_move state
   | exception Board.SquareNotFound ->
       ();
       next_move state);
  ()

let play_game board =
  let state = State.init_state board in
  next_move state

let init_window =
  open_graph " 1000x700";
  set_window_title "Dama"

let rec loop () = loop ()

let () =
  init_window;
  let initial = Board.game_init 8 in
  play_game initial;
  loop ()
