open Graphics
open Png
open Graphic_image

let display_image img_path x y =
  let img = Png.load_as_rgb24 img_path [] in
  let graphics_img = Graphic_image.of_image img in
  Graphics.draw_image graphics_img x y

let display_board board =
  let b = Board.get_board board in
  let x_offset = 100 in
  let y_offset = 50 in
  let spacing = 82 in
  let rec print_board x y = function
    | [] -> ()
    | sq_lst :: remaining_rows ->
        let rec print_row x y = function
          | [] -> print_board x_offset (y + spacing) remaining_rows
          | sq :: remaining_sqs -> (
              let piece = Board.get_occupant sq in
              match piece with
              | None ->
                  display_image "images/empty-square.png" x y;
                  print_row (x + spacing) y remaining_sqs
              | Some p ->
                  let c, _ = Board.get_piece_info sq in
                  if c = Board.Black then (
                    display_image "images/black-w-background.png" x y;
                    print_row (x + spacing) y remaining_sqs)
                  else display_image "images/white-w-background.png" x y;
                  print_row (x + spacing) y remaining_sqs)
        in
        print_row x y sq_lst
  in

  print_board x_offset y_offset b

let init_window =
  open_graph " 1000x800";
  set_window_title "Dama"

let rec loop () = loop ()

let () =
  init_window;
  let initial = Board.game_init 8 in
  display_board initial;
  loop ()
