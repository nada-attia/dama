open Graphics
open Png
open Graphic_image

let display_image img_path x y =
  let img = Png.load_as_rgb24 img_path [] in
  let graphics_img = Graphic_image.of_image img in
  Graphics.draw_image graphics_img x y

let display_images () =
  display_image "images/black-w-background.png" 0 0;
  display_image "images/white-w-background.png" 0 82

let init_window =
  open_graph " 1000x800";
  set_window_title "Dama"

let rec loop () = loop ()

let () =
  init_window;
  display_images ();
  loop ()
