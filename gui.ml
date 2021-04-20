open Graphics
open Png
open Graphic_image
open Jpeg

let init_window =
  open_graph "";
  set_window_title "Dama";
  let img = Jpeg.load "images/black-piece.jpeg" [] in
  let graphics_img = Graphic_image.of_image img in
  Graphics.draw_image graphics_img 0 0

let rec loop () = loop ()

let () =
  init_window;
  loop ()
