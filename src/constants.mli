(** Constants*)

(** The width of the GUI window *)
val window_width : int

(** The height of the GUI window*)
val window_height : int

(** The horizontal margin between the start of the GUI window and the
    start of the board *)
val x_margin : int

(** The vertical margin between the start of the GUI window and the
    start of the board *)
val y_margin : int

(** The size of the squares on the board *)
val square_size : int

(** The distance between the start of one square and the start of the
    other *)
val square_offset : int

(** The size of the board *)
val board_size : int

(** The number of levels traversed in the Monte Carlo Tree Search*)
val ai_level : int

(** The x location where error messages are displayed *)
val error_x : int

(** The y location where error messages are displayed *)
val error_y : int
