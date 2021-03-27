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

let label_hd = function () -> failwith "Empty label." | hd, _ -> hd

let label_tl = function () -> failwith "Empty label." | _, tl -> tl

let get_square labl brd = brd.board

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
