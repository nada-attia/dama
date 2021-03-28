type terminal_rep = {
  white_square_empty : char;
  white_square_white : char;
  white_square_black : char;
  black_square_empty : char;
  black_square_black : char;
  black_square_white : char;
}

type color =
  | Black
  | White

type role =
  | Man
  | Lady

type piece = {
  color : color;
  mutable role : role;
}

type square = {
  color : color;
  occupant : piece option;
  mutable label : char * int;
}

type side_board = {
  man_count : int;
  lady_count : int;
}

type t = {
  board : square list list;
  w_side_board : side_board;
  b_side_board : side_board;
  size : int;
}

exception EmptyStartSquare

let get_init_player = White

(* Auxillary helper function to handle each item of the list. *)
let de_opt_aux = function None -> [] | Some s -> [ s ]

(*Helper function to remove options from list. *)
let deoptional_lst lst = List.concat @@ List.map de_opt_aux lst

(* Helper function to get the square given the label it corresponds to
   and the board it is in. *)
let get_square labl (brd : t) =
  let col_ltr, num_pos = labl in
  let row = num_pos - 1 in
  let row_lst = List.nth brd.board row in
  let col = Char.code col_ltr - 97 in
  List.nth row_lst col

(* Helper function to get list of square left of [square] if exists,
   else []. Behavior varies dependent on color since orientation of
   board changes. Uses list c_val determined by color [color] to perform
   operations. *)
let square_left sq color board =
  let ltr_pos, num_pos = sq.label in
  let ltr_pos_num = Char.code ltr_pos in
  let c_vals =
    if color = White then [| ltr_pos_num - 1; Char.code 'a'; -1 |]
    else [| Char.code 'h'; ltr_pos_num + 1; 1 |]
  in

  if c_vals.(0) >= c_vals.(1) then
    let left1_label = (Char.chr (ltr_pos_num - c_vals.(2)), num_pos) in
    [ get_square left1_label board ]
  else []

(* Helper function to get list of square right of [square] if exists,
   else []. Behavior varies dependent on color since orientation of
   board changes. Uses list c_val determined by color [color] to perform
   operations. *)
let square_right sq color board =
  let ltr_pos, num_pos = sq.label in
  let ltr_pos_num = Char.code ltr_pos in
  let c_vals =
    if color = White then [| ltr_pos_num + 1; Char.code 'h'; 1 |]
    else [| Char.code 'a'; ltr_pos_num - 1; -1 |]
  in

  if c_vals.(0) <= c_vals.(1) then
    let right1_label = (Char.chr (ltr_pos_num + c_vals.(2)), num_pos) in
    [ get_square right1_label board ]
  else []

(* Helper function to get list of square above of [square] if exists,
   else []. Behavior varies dependent on color since orientation of
   board changes. Uses list c_val determined by color [color] to perform
   operations. *)
let square_above sq color board =
  let ltr_pos, num_pos = sq.label in
  let c_vals =
    if color = White then [| num_pos + 1; 8; 1 |]
    else [| 1; num_pos - 1; -1 |]
  in
  if c_vals.(0) <= c_vals.(1) then
    let above1_label = (ltr_pos, num_pos + c_vals.(2)) in
    [ get_square above1_label board ]
  else []

(* Helper function to get list of square below of [square] if exists,
   else []. Behavior varies dependent on color since orientation of
   board changes. Uses list c_val determined by color [color] to perform
   operations. *)
let square_below sq color board =
  let ltr_pos, num_pos = sq.label in
  let c_vals =
    if color = White then [| num_pos - 1; 1; 1 |]
    else [| 8; num_pos + 1; -1 |]
  in
  if c_vals.(0) >= c_vals.(1) then
    let above1_label = (ltr_pos, num_pos - c_vals.(2)) in
    [ get_square above1_label board ]
  else []

(* Helper function to check to see if a given square is occupied or not.*)
let check_if_occupied square =
  if square.occupant = None then false else true

(* Helper function [get_vacant func square board] is the square option
   on top of, left of, or right of square [square] from board [board],
   depending on which helper function [func] is passed in. If the square
   is vacant, it will be returned, otherwise None. *)
let get_vacant func square color board =
  let res_sqs = func square color board in
  try
    let res_sqr = List.nth res_sqs 0 in
    if check_if_occupied res_sqr then None else Some res_sqr
  with _ -> None

(* Helper function to get the neighbor squares that can be moved to from
   a given square noting the color of the piece on said square, assuming
   it is a regular non-Lady piece. *)
let get_movable_squares_reg square color board =
  let squares = [] in
  let squares =
    if get_vacant square_above square color board <> None then
      get_vacant square_above square color board :: squares
    else squares
  in
  let squares =
    if get_vacant square_right square color board <> None then
      get_vacant square_right square color board :: squares
    else squares
  in
  let squares =
    if get_vacant square_left square color board <> None then
      get_vacant square_left square color board :: squares
    else squares
  in
  squares

(* [get_jumps_dir sq brd clr fun] gets the jump in a given direction
   guided by function [func] for a piece of color [clr] on square [sq]
   on board [brd]*)
let get_jumps_dir sq (brd : t) clr func =
  (* Get next square in direction *)
  let nxt_sq = func sq clr brd in
  (* If we got something... *)
  if nxt_sq <> [] then
    (* Extract square from list *)
    let nxt_sq = List.nth nxt_sq 0 in
    (* Get 2 squares next in given direction from sq *)
    let nxt_2 = func nxt_sq clr brd in
    (* If we got something... *)
    if nxt_2 <> [] then
      (* Extract the square from the list *)
      let nxt_2 = List.nth nxt_2 0 in
      (* If the next square is occupied and the 2nd next square is empty*)
      if
        get_vacant func sq clr brd = None
        && get_vacant func nxt_2 clr brd <> None
        (* get the piece from the piece option *)
      then
        let pc_abv = Option.get nxt_sq.occupant in
        (* ensure that the piece color is enemy color*)
        if pc_abv.color <> clr then [ nxt_sq ] else []
        (* conditions for jump not met. *)
      else []
      (* Did not find next square or 2nd next in given direction. *)
    else []
  else []

(* [get_all_jumps sq brd clr] describes the list of squares that
   represent all possible jumps avalible for the piece of color [clrf]
   on square [sq] on board [brd] *)
let get_all_jumps sq brd clr =
  let above = get_jumps_dir sq brd clr square_above in
  let left = get_jumps_dir sq brd clr square_left in
  let right = get_jumps_dir sq brd clr square_right in
  above @ left @ right

(* [get_all_vac_sq_dir acc sq clr brd func] is the list of squares that
   are vacant in direction determined by seeking function [func] of
   square [sq] given a piece of color [clr] occupying said sqaure [sq]
   on board [brd]. Function is tail recursive with accumulator list
   [acc]*)
let rec get_all_vac_sq_dir acc sq (clr : color) (brd : t) func =
  let next = get_vacant func sq clr brd in
  match next with
  | Some n_sqr -> get_all_vac_sq_dir (n_sqr :: acc) n_sqr clr brd func
  | None -> acc

let get_all_jumps_lady sq brd clr = failwith "f"

let where_move brd sq =
  try
    let pc = Option.get sq.occupant in
    let pc_typ = pc.role in
    (* If man piece *)
    if pc_typ = Man then
      (* If jump is avalible, it must be taken. *)
      if get_all_jumps sq brd pc.color = [] then
        deoptional_lst (get_movable_squares_reg sq pc.color brd)
      else get_all_jumps sq brd pc.color (* If lady piece *)
    else []
  with _ -> raise EmptyStartSquare

let can_move square board turn =
  let condition1 = check_if_occupied square in
  let pc = Option.get square.occupant in
  let condition2 = pc.color = turn in
  let condition3 = where_move board square <> [] in
  condition1 && condition2 && condition3

let terminal_rep_string =
  {
    white_square_empty = '.';
    white_square_white = 'w';
    white_square_black = 'B';
    black_square_empty = ' ';
    black_square_black = 'b';
    black_square_white = 'W';
  }

(** [init_row n row piece_opt acc] initializes [row] of length [n] with
    [piece_opt]*)
let rec init_row n row piece_opt acc =
  match n with
  | 0 -> acc
  | _ ->
      let square =
        match acc with
        | [] ->
            let color = if row mod 2 = 1 then Black else White in
            {
              color;
              occupant = piece_opt;
              label =
                (let last_col_char =
                   char_of_int (int_of_char 'a' + n - 1)
                 in
                 (last_col_char, row));
            }
        | h :: t ->
            let color = if h.color = Black then White else Black in
            let prev_letter =
              match h.label with
              | c, r -> char_of_int (int_of_char c - 1)
            in
            { color; occupant = piece_opt; label = (prev_letter, row) }
      in
      init_row (n - 1) row piece_opt (square :: acc)

(** [board_init_helper n row row_end piece_opt acc] initializes rows of
    length [n] starting at [row] for [count] rows with [piece_opt]*)
let rec board_init_helper n row count piece_opt acc =
  match n with
  | 0 -> acc
  | _ -> (
      match row with
      | 0 -> acc
      | _ ->
          if count = 0 then acc
          else
            board_init_helper n (row - 1) (count - 1) piece_opt
              (init_row n row piece_opt [] :: acc))

let board_init n =
  [ init_row n 1 None [] ]
  @ [ init_row n 2 (Some { color = White; role = Man }) [] ]
  @ [ init_row n 3 (Some { color = White; role = Man }) [] ]
  @ board_init_helper n 4 (n - 6) None []
  @ [ init_row n (n - 2) (Some { color = Black; role = Man }) [] ]
  @ [ init_row n (n - 1) (Some { color = Black; role = Man }) [] ]
  @ [ init_row n n None [] ]

let game_init n =
  let side_board = { man_count = 2 * n; lady_count = 0 } in
  {
    board = board_init n;
    w_side_board = side_board;
    b_side_board = side_board;
    size = n;
  }

let get_board t = t.board

let count_inactive curr_board p_color =
  match p_color with
  | Black ->
      (2 * curr_board.size)
      - (curr_board.b_side_board.lady_count
       + curr_board.b_side_board.man_count)
  | White ->
      (2 * curr_board.size)
      - (curr_board.w_side_board.lady_count
       + curr_board.w_side_board.man_count)

let square_to_string (square : square) : string =
  match square.color with
  | White -> (
      match square.occupant with
      | None -> Char.escaped terminal_rep_string.white_square_empty
      | Some p -> (
          match p.color with
          | Black -> Char.escaped terminal_rep_string.white_square_black
          | White -> Char.escaped terminal_rep_string.white_square_white
          ))
  | Black -> (
      match square.occupant with
      | None -> Char.escaped terminal_rep_string.black_square_empty
      | Some p -> (
          match p.color with
          | Black -> Char.escaped terminal_rep_string.black_square_black
          | White -> Char.escaped terminal_rep_string.black_square_white
          ))

let string_of_row r row =
  List.fold_left
    (fun acc square -> "| " ^ square_to_string square ^ " " ^ acc)
    ("|" ^ string_of_int row)
    r

let rec terminal_rep_string_helper t count =
  match t with
  | [] -> ""
  | h :: t ->
      string_of_row h count ^ "\n"
      ^ terminal_rep_string_helper t (count + 1)

let rec col_label_string count n =
  if count <= n then
    let str =
      char_of_int (int_of_char 'a' - 1 + count) |> Char.escaped
    in
    "  " ^ str ^ " " ^ col_label_string (count + 1) n
  else " \n"

let terminal_rep_string t count =
  col_label_string count t.size
  ^ terminal_rep_string_helper t.board count

let find_square board label =
  match board.board with
  | [] -> None
  | row :: remaining_rows ->
      let rec get_square r =
        match r with
        | [] -> None
        | square :: remaining_squares ->
            if square.label = label then Some square
            else get_square remaining_squares
      in
      get_square row
