open Graphics

let init_window =
  open_graph "";
  set_window_title "Dama";
  let rec loop () = loop () in
  loop ()

let () = init_window
