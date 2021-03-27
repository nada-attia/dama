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
}

type current =
  | InProgress
  | Finished
  | Pregame

type state = {
  turn : color;
  board : t;
  current : current;
}

let update_state board command = failwith "Unimplemented"

let de_opt_aux = function None -> [] | Some s -> [ s ]

(*Helper function to remove options from list. *)
let deoptional_lst lst = List.concat @@ List.map de_opt_aux lst

(* Helper function to get the head of the label. *)
let label_hd = function hd, _ -> hd

(* Helper function to get the tail of the label. *)
let label_tl = function _, tl -> tl

(* Helper function to get the square given the label it corresponds to
   and the board it is in. *)
let get_square labl (brd : t) =
  let row = label_tl labl - 1 in
  let row_lst = List.nth brd.board row in
  let col_ltr = label_hd labl in
  let col = Char.code col_ltr - 97 in
  List.nth row_lst col

(* Helper function to get list of left square to [square] if exists,
   else []. *)
let square_left sq board =
  let labl = sq.label in
  let ltr_pos = label_hd labl in
  let ltr_pos_num = Char.code ltr_pos in
  let num_pos = label_tl labl in
  if ltr_pos_num - 1 >= 97 then
    let left1_label = (Char.chr (ltr_pos_num - 1), num_pos) in
    [ get_square left1_label board ]
  else []

(* Helper function to get list of right square to [square] if exists,
   else []. *)
let square_right sq board =
  let labl = sq.label in
  let ltr_pos = label_hd labl in
  let ltr_pos_num = Char.code ltr_pos in
  let num_pos = label_tl labl in
  if ltr_pos_num + 1 <= 104 then
    let right1_label = (Char.chr (ltr_pos_num + 1), num_pos) in
    [ get_square right1_label board ]
  else []

(* Helper function to get square above of [square] if exists, else None. *)
let square_above sq board =
  let labl = sq.label in
  let ltr_pos = label_hd labl in
  let num_pos = label_tl labl in
  if num_pos + 1 <= 8 then
    let above1_label = (ltr_pos, num_pos + 1) in
    [ get_square above1_label board ]
  else []

(* Helper function to check to see if a given square is occupied or not.*)
let check_if_occupied square =
  if square.occupant = None then false else true

let is_valid_label lbl =
  if Char.code (label_hd lbl) > 96 && Char.code (label_hd lbl) < 105
  then if label_tl lbl < 9 && label_tl lbl > 0 then true else false
  else false

(* Helper function [get_vacant func square board] is the square option
   on top of, left of, or right of square [square] from board [board],
   depending on which helper function [func] is passed in. If the square
   is vacant, it will be returned, otherwise None. *)
let get_vacant func square board =
  let res_sqs = func square board in
  try
    let res_sqr = List.nth res_sqs 0 in
    if check_if_occupied res_sqr then None else Some res_sqr
  with _ -> None

(* Helper function to get the neighbor squares that can theoretically be
   moved to from a given square noting the color of the piece on said
   square, assuming it is a regular non-Lady piece. *)
let get_movable_squares_reg square color board =
  let squares = [] in
  let squares =
    if get_vacant square_above square board <> None then
      get_vacant square_above square board :: squares
    else squares
  in
  let squares =
    if get_vacant square_right square board <> None then
      get_vacant square_right square board :: squares
    else squares
  in
  let squares =
    if get_vacant square_left square board <> None then
      get_vacant square_left square board :: squares
    else squares
  in
  squares

let un_option = function
  | Some s -> s
  | None -> failwith "non-existent square"

(* [get_jumps sq brd clr func] describes all possible locations the
   occupant of square [square] can jump as a list of squares going in
   the direction helper function [func] describes. (Recall that possible
   jumps are required to be taken in Dama, but 2 jumps could be an
   option.) *)
let get_jumps_d sq brd clr func =
  (* Get next square in direction *)
  let nxt_sq = func sq brd in
  (* If we got something... *)
  if nxt_sq <> [] then
    (* Extract square from list *)
    let nxt_sq = List.nth nxt_sq 0 in
    (* Get 2 squares next in given direction from sq *)
    let nxt_2 = func nxt_sq brd in
    (* If we got something... *)
    if nxt_2 <> [] then
      (* Extract the square from the list *)
      let nxt_2 = List.nth nxt_2 0 in
      (* If the next square is occupied and the 2nd next square is empty*)
      if
        get_vacant func sq brd = None
        && get_vacant func nxt_2 brd <> None
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

let get_all_jumps sq brd clr =
  let above = get_jumps_d sq brd clr square_above in
  let left = get_jumps_d sq brd clr square_left in
  let right = get_jumps_d sq brd clr square_right in
  above @ left @ right

(* [where_move board square] describes all of the legal locations the
   occupant of square [square] can move to. *)
let where_move brd sq st =
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
  with _ ->
    print_endline "Sorry, no piece is there.";
    []

(* [can_move square board st] describes if a piece on square [square]
   can move on board [board] for state [st]. Ensures the square is not
   empty and occupied, that the current occupant has the turn in the
   state, and that there are squares it can legally move to. *)
let can_move square board (st : state) =
  let condition1 = not (check_if_occupied square) in
  if square.occupant <> None then
    let pc = Option.get square.occupant in
    let condition2 = pc.color = st.turn in
    let condition3 = where_move board square st <> [] in
    condition1 && condition2 && condition3
  else false

let terminal_rep_string t =
  {
    white_square_empty = ' ';
    white_square_white = ' ';
    white_square_black = ' ';
    black_square_empty = ' ';
    black_square_black = ' ';
    black_square_white = ' ';
  }

let board_init n = failwith "Unimplemented"

let count_active curr_board p_color = failwith "Unimplemented"

let count_inactive curr_board p_color = failwith "Unimplemented"
