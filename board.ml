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
            { color; occupant = piece_opt; label = ('H', row) }
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
    length [n] starting at [row] and ending at [row_end] with
    [piece_opt]*)
let rec board_init_helper n row row_end piece_opt acc =
  match n with
  | 0 -> acc
  | _ -> (
      match row with
      | 0 -> acc
      | _ ->
          if row = row_end then acc
          else
            board_init_helper n (row - 1) row_end piece_opt
              (init_row n row piece_opt [] :: acc))

let board_init n =
  [ init_row n 1 None [] ]
  @ [ init_row n 2 (Some { color = White; role = Man }) [] ]
  @ [ init_row n 3 (Some { color = White; role = Man }) [] ]
  @ board_init_helper n 4 (n - 6) None []
  @ [ init_row n (n - 2) (Some { color = Black; role = Man }) [] ]
  @ [ init_row n (n - 1) (Some { color = Black; role = Man }) [] ]
  @ [ init_row n n None [] ]

let count_inactive curr_board p_color = failwith "Unimplemented"

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

let string_of_row r =
  List.fold_left
    (fun acc square -> "| " ^ square_to_string square ^ " " ^ acc)
    "|" r

let rec terminal_rep_string t =
  match t with
  | [] -> ""
  | h :: t -> string_of_row h ^ "\n" ^ terminal_rep_string t
