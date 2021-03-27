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

(* Helper function to get the head of the label. *)
let label_hd = function hd, _ -> hd

(* Helper function to get the tail of the label. *)
let label_tl = function _, tl -> tl

(* Helper function to get the square given the label it corresponds to
   and the board it is in. *)
let get_square labl brd =
  let row = label_tl labl - 1 in
  let row_lst = List.nth brd.board row in
  let col_ltr = label_hd labl in
  let col = Char.code col_ltr - 97 in
  List.nth row_lst col

(* Helper function to get square left of [square] if exists, else None. *)
let square_left sq board =
  let labl = sq.label in
  let ltr_pos = label_hd labl in
  let ltr_pos_num = Char.code ltr_pos in
  let num_pos = label_tl labl in
  if ltr_pos_num - 1 >= 97 then
    let left1_label = (Char.chr (ltr_pos_num - 1), num_pos) in
    Some (get_square left1_label board)
  else None

(* Helper function to get square right of [square] if exists, else None. *)
let square_right sq board =
  let labl = sq.label in
  let ltr_pos = label_hd labl in
  let ltr_pos_num = Char.code ltr_pos in
  let num_pos = label_tl labl in
  if ltr_pos_num + 1 <= 104 then
    let right1_label = (Char.chr (ltr_pos_num + 1), num_pos) in
    Some (get_square right1_label board)
  else None

(* Helper function to get square above of [square] if exists, else None. *)
let square_above sq board =
  let labl = sq.label in
  let ltr_pos = label_hd labl in
  let num_pos = label_tl labl in
  if num_pos + 1 <= 8 then
    let above1_label = (ltr_pos, num_pos + 1) in
    Some (get_square above1_label board)
  else None

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
