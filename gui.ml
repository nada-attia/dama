open Graphics
open Graphic_image
open Png
open Constants

let display_image img_path x y =
  let img = Png.load_as_rgb24 img_path [] in
  let graphics_img = Graphic_image.of_image img in
  Graphics.draw_image graphics_img x y

let display_board board =
  let b = List.rev (Board.get_board board) in
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
  if y >= lower_8 && y <= lower_8 + square_size then "8"
  else if y >= lower_7 && y <= lower_7 + square_size then "7"
  else if y >= lower_6 && y <= lower_6 + square_size then "6"
  else if y >= lower_5 && y <= lower_5 + square_size then "5"
  else if y >= lower_4 && y <= lower_4 + square_size then "4"
  else if y >= lower_3 && y <= lower_3 + square_size then "3"
  else if y >= lower_2 && y <= lower_2 + square_size then "2"
  else if y >= lower_1 && y <= lower_1 + square_size then "1"
  else ""

let get_x_pos x =
  if x >= lower_a && x <= lower_a + square_size then "a"
  else if x >= lower_b && x <= lower_b + square_size then "b"
  else if x >= lower_c && x <= lower_c + square_size then "c"
  else if x >= lower_d && x <= lower_d + square_size then "d"
  else if x >= lower_e && x <= lower_e + square_size then "e"
  else if x >= lower_f && x <= lower_f + square_size then "f"
  else if x >= lower_g && x <= lower_g + square_size then "g"
  else if x >= lower_h && x <= lower_h + square_size then "h"
  else ""

let get_board_pos x y =
  let row_pos = get_y_pos y in
  let col_pos = get_x_pos x in
  let pos = col_pos ^ row_pos in
  if String.length pos = 2 then pos else ""

let get_mouse_click () =
  let event = wait_next_event [ Button_down ] in
  get_board_pos event.mouse_x event.mouse_y

let rec next_move state =
  (let b = State.get_board state in
   display_board b;
   let start_pos = get_mouse_click () in
   let end_pos = get_mouse_click () in
   let command = "move " ^ start_pos ^ " " ^ end_pos in
   let new_state = State.update_state state (Command.parse command) in
   match new_state with
   | state -> next_move state
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
       ();
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
  open_graph " 1000x800";
  set_window_title "Dama"

let rec loop () = loop ()

let () =
  init_window;
  let initial = Board.game_init 8 in
  play_game initial;
  loop ()
