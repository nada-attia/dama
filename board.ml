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
  let row = label_tl labl in
  let row_lst = List.nth brd.board row in
  let col = label_hd labl in
  List.nth row_lst col

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
