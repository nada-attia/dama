type terminal_rep = {
  white_square_empty : char;
  white_square_white : char;
  white_square_black : char;
  black_square_empty : char;
  black_square_black : char;
  black_square_white : char;
  white_square_white_dama : char;
  white_square_black_dama : char;
  black_square_white_dama : char;
  black_square_black_dama : char;
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

type direction =
  | Up
  | Down
  | Left
  | Right

type square = {
  color : color;
  mutable occupant : piece option;
  mutable label : char * int;
}

type side_board = {
  mutable man_count : int;
  mutable lady_count : int;
}

type t = {
  board : square list list;
  mutable w_side_board : side_board;
  mutable b_side_board : side_board;
  size : int;
}

exception EmptyStartSquare

exception NoPiece

exception SquareNotFound

let get_board t = t.board

let get_init_player = White

let get_other_player color = if color = White then Black else White

let rec get_sqlst_label = function
  | [] -> []
  | h :: t -> h.label :: get_sqlst_label t

let get_label sq = sq.label

let get_square labl (brd : t) =
  let squares = List.flatten brd.board in
  let rec find_square = function
    | [] -> raise SquareNotFound
    | h :: t -> if h.label = labl then h else find_square t
  in
  find_square squares

(* Helper function to get list of square left of [square] if exists,
   else []. Behavior varies dependent on color since orientation of
   board changes. Uses list c_val determined by color [color] to perform
   operations. *)
let square_left sq color board =
  let ltr_pos, num_pos = sq.label in
  let ltr_pos_num = Char.code ltr_pos in
  let c_vals =
    if color = White then [| ltr_pos_num - 1; Char.code 'a'; 1 |]
    else [| Char.code 'h'; ltr_pos_num + 1; -1 |]
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

let get_square_dir sq (board : t) (color : color) = function
  | Up -> square_above sq color board
  | Down -> square_below sq color board
  | Left -> square_left sq color board
  | Right -> square_right sq color board

(* Helper function to check to see if a given square is occupied or not.*)
let check_if_occupied square =
  if square.occupant = None then false else true

(* Helper function to get the neighbor squares that can be moved to from
   a given square noting the color of the piece on said square, assuming
   it is a regular non-Lady piece. *)
let get_movable_squares_reg square (color : color) (board : t) =
  let squares =
    get_square_dir square board color Up
    @ get_square_dir square board color Right
    @ get_square_dir square board color Left
  in
  List.filter (fun square -> square.occupant = None) squares

(* [get_jumps_dir sq brd clr fun] gets the square to jump to in a given
   direction guided by function [func] for a piece of color [clr] on
   square [sq] on board [brd]*)
let get_jumps_dir sq (brd : t) clr direction =
  (* Get next square in direction *)
  let nxt_sq = get_square_dir sq brd clr direction in
  (* If we got something... *)
  if nxt_sq <> [] then
    (* Extract square from list *)
    let nxt_sq = List.nth nxt_sq 0 in
    (* Get 2 squares next in given direction from sq *)
    let nxt_2 = get_square_dir nxt_sq brd clr direction in
    (* If we got something... *)
    if nxt_2 <> [] then
      (* Extract the square from the list *)
      let nxt_2 = List.nth nxt_2 0 in
      (* If the next square is occupied and the 2nd next square is empty*)
      if
        nxt_sq.occupant <> None && nxt_2.occupant = None
        (* get the piece from the piece option *)
      then
        let pc_abv = Option.get nxt_sq.occupant in
        (* ensure that the piece color is enemy color*)
        if pc_abv.color <> clr then [ (nxt_sq, nxt_2) ] else []
        (* conditions for jump not met. *)
      else []
      (* Did not find next square or 2nd next in given direction. *)
    else []
  else []

(* [get_all_jumps sq brd clr] describes the list of squares that
   represent all possible jump destination avalible for the piece of
   color [clrf] on square [sq] on board [brd] *)
let get_all_jumps sq brd clr =
  let above = get_jumps_dir sq brd clr Up in
  let left = get_jumps_dir sq brd clr Left in
  let right = get_jumps_dir sq brd clr Right in
  above @ left @ right

(* Helper function [get_vacant func square board] is the square option
   on top of, left of, or right of square [square] from board [board],
   depending on which helper function [func] is passed in. If the square
   is vacant, it will be returned, otherwise None. *)
let get_vacant square color board direction =
  let res_sqs = get_square_dir square board color direction in
  try
    let res_sqr = List.nth res_sqs 0 in
    if check_if_occupied res_sqr then None else Some res_sqr
  with _ -> None

(* [get_all_vac_sq_dir acc sq clr brd func] is the list of squares that
   are vacant in direction determined by seeking function [func] of
   square [sq] given a piece of color [clr] occupying said sqaure [sq]
   on board [brd]. Function is tail recursive with accumulator list
   [acc]*)
let rec get_all_vac_sq_dir acc sq (clr : color) (brd : t) direction =
  let next = get_vacant sq clr brd direction in
  match next with
  | Some n_sqr ->
      get_all_vac_sq_dir (n_sqr :: acc) n_sqr clr brd direction
  | None -> acc

(* Returns list of single square in given direction that can be jumped
   to for lady piece else [].*)
let get_all_jumps_dir_lady sq brd clr func =
  let vacants = get_all_vac_sq_dir [] sq clr brd func in
  (* First of the list is, is the last vacant in given direction, so
     jumps of last vacant square. *)
  if vacants <> [] then get_jumps_dir (List.nth vacants 0) brd clr func
  else []

(* [get_all_jumps_lady sq brd clr] describes all of the squares the lady
   piece occupying square [sq] of color [clr] on board [brd] has
   avalible to it.*)
let get_all_jumps_lady sq brd clr =
  let above = get_all_jumps_dir_lady sq brd clr Up in
  let below = get_all_jumps_dir_lady sq brd clr Down in
  let right = get_all_jumps_dir_lady sq brd clr Right in
  let left = get_all_jumps_dir_lady sq brd clr Left in
  above @ below @ right @ left

let get_movable_squares_lady square color board =
  let squares = [] in
  let squares =
    if get_all_vac_sq_dir [] square color board Up <> [] then
      get_all_vac_sq_dir [] square color board Up @ squares
    else squares
  in
  let squares =
    if get_all_vac_sq_dir [] square color board Right <> [] then
      get_all_vac_sq_dir [] square color board Right @ squares
    else squares
  in
  let squares =
    if get_all_vac_sq_dir [] square color board Left <> [] then
      get_all_vac_sq_dir [] square color board Left @ squares
    else squares
  in
  squares

let rec final_squares = function
  | [] -> []
  | (captured_sq, final_sq) :: t -> final_sq :: final_squares t

(* Returns a list of squares a piece on square [sq] can move to on board
   [brd]*)
let where_move brd sq =
  try
    let pc = Option.get sq.occupant in
    let pc_typ = pc.role in
    (* If man piece *)
    if pc_typ = Man then
      (* If jump is avalible, it must be taken. But if not... *)
      let jumps = get_all_jumps sq brd pc.color in
      if final_squares jumps = [] then
        get_movable_squares_reg sq pc.color brd
      else final_squares jumps (* If lady piece *)
    else if
      (* If jump is avalible, it must be taken. But if not... *)
      get_all_jumps_lady sq brd pc.color = []
    then
      let lady_jumps = get_all_jumps_lady sq brd pc.color in
      final_squares lady_jumps
    else get_movable_squares_lady sq pc.color brd
  with _ -> raise EmptyStartSquare

let can_move square board turn =
  let condition1 = check_if_occupied square in
  match square.occupant with
  | None -> false
  | Some pc ->
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
    white_square_white_dama = 'm';
    white_square_black_dama = 'P';
    black_square_white_dama = 'M';
    black_square_black_dama = 'p';
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

let board_init n =
  [ init_row n 1 None [] ]
  @ [ init_row n 2 (Some { color = White; role = Man }) [] ]
  @ [ init_row n 3 (Some { color = White; role = Man }) [] ]
  @ [ init_row n 4 None [] ]
  @ [ init_row n 5 None [] ]
  @ [ init_row n 6 (Some { color = Black; role = Man }) [] ]
  @ [ init_row n 7 (Some { color = Black; role = Man }) [] ]
  @ [ init_row n 8 None [] ]

let game_init n =
  let side_board = { man_count = 0; lady_count = 0 } in
  {
    board = board_init n;
    w_side_board = side_board;
    b_side_board = side_board;
    size = n;
  }

let get_board t = t.board

let get_color i =
  if i = 1 then Black
  else if i = 2 then White
  else failwith "Enter 1 or 2"

let count_inactive curr_board p_color =
  match p_color with
  | Black ->
      curr_board.b_side_board.lady_count
      + curr_board.b_side_board.man_count
  | White ->
      curr_board.w_side_board.lady_count
      + curr_board.w_side_board.man_count

let square_to_string (square : square) : string =
  match square.color with
  | White -> (
      match square.occupant with
      | None -> Char.escaped terminal_rep_string.white_square_empty
      | Some p -> (
          match p.color with
          | Black ->
              if p.role = Man then
                Char.escaped terminal_rep_string.white_square_black
              else
                Char.escaped terminal_rep_string.white_square_black_dama
          | White ->
              if p.role = Man then
                Char.escaped terminal_rep_string.white_square_white
              else
                Char.escaped terminal_rep_string.white_square_white_dama
          ))
  | Black -> (
      match square.occupant with
      | None -> Char.escaped terminal_rep_string.black_square_empty
      | Some p -> (
          match p.color with
          | Black ->
              if p.role = Man then
                Char.escaped terminal_rep_string.black_square_black
              else
                Char.escaped terminal_rep_string.black_square_black_dama
          | White ->
              if p.role = Man then
                Char.escaped terminal_rep_string.black_square_white
              else
                Char.escaped terminal_rep_string.black_square_white_dama
          ))

let string_of_row r row =
  List.fold_right
    (fun square acc -> "| " ^ square_to_string square ^ " " ^ acc)
    r
    ("|" ^ string_of_int row)

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

let get_piece (sq : square) =
  let piece = sq.occupant in
  match piece with None -> raise NoPiece | Some piece -> piece

let get_piece_info (sq : square) =
  try
    let p = get_piece sq in
    (p.color, p.role)
  with exn -> raise NoPiece

let remove_pieces captured_sq turn board =
  let captured_piece = get_piece captured_sq in
  let role = captured_piece.role in
  captured_sq.occupant <- None;
  let white_side = board.w_side_board in
  let black_side = board.b_side_board in
  if turn = White then
    if role = Lady then
      white_side.lady_count <- white_side.lady_count + 1
    else white_side.man_count <- white_side.man_count + 1
  else if role = Lady then
    black_side.lady_count <- black_side.lady_count + 1
  else black_side.man_count <- black_side.man_count + 1

let rec update_board turn captured board start_pos end_pos =
  let start_sq = get_square start_pos board in
  let end_sq = get_square end_pos board in
  end_sq.occupant <- Some (get_piece start_sq);
  start_sq.occupant <- None;
  match captured with
  | None -> ()
  | Some sq -> remove_pieces sq turn board
