type terminal_rep = {
  empty : char;
  white : char;
  black : char;
  white_dama : char;
  black_dama : char;
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
  mutable can_jump : bool;
}

type square = {
  mutable occupant : piece option;
  label : char * int;
}

type side_board = {
  mutable man_count : int;
  mutable lady_count : int;
}

type t = {
  board : square list list;
  w_side_board : side_board;
  b_side_board : side_board;
  size : int;
}

exception EmptyStartSquare

exception NoPiece

let get_board t = t.board

let get_init_player = White

let get_other_player color = if color = White then Black else White

let rec get_sqlst_label = function
  | [] -> []
  | h :: t -> h.label :: get_sqlst_label t

let get_label sq = sq.label

let get_occupant sq = sq.occupant

let get_color (piece : piece) = piece.color

let try_upgrade_piece sqr (brd : t) (clr : color) =
  let ltr, num = sqr.label in
  let pc =
    match sqr.occupant with
    | None -> raise EmptyStartSquare
    | Some s -> s
  in
  (* if white, end of board is num=8 *)
  if clr = White then
    if num = 8 then pc.role <- Lady
    else () (* else is black so end is num=1 *)
  else if num = 1 then pc.role <- Lady
  else ()

let terminal_rep_string =
  {
    empty = '.';
    white = 'w';
    black = 'b';
    white_dama = 'm';
    black_dama = 'p';
  }

let init_piece (piece : piece option) () =
  match piece with
  | None -> None
  | Some p -> Some { color = p.color; role = p.role; can_jump = false }

let init_piece_deop p () =
  { color = p.color; role = p.role; can_jump = false }

(** [init_row n row piece_opt acc] initializes [row] of length [n] with
    [piece_opt]*)
let rec init_row n row piece_opt acc =
  match n with
  | 0 -> acc
  | _ ->
      let square =
        match acc with
        | [] ->
            {
              occupant = init_piece piece_opt ();
              label =
                (let last_col_char =
                   char_of_int (int_of_char 'a' + n - 1)
                 in
                 (last_col_char, row));
            }
        | h :: t ->
            let prev_letter =
              match h.label with
              | c, r -> char_of_int (int_of_char c - 1)
            in
            {
              occupant = init_piece piece_opt ();
              label = (prev_letter, row);
            }
      in
      init_row (n - 1) row piece_opt (square :: acc)

let board_init n =
  [ init_row n 1 None [] ]
  @ [
      init_row n 2
        (Some { color = White; role = Man; can_jump = false })
        [];
    ]
  @ [
      init_row n 3
        (Some { color = White; role = Man; can_jump = false })
        [];
    ]
  @ [ init_row n 4 None [] ]
  @ [ init_row n 5 None [] ]
  @ [
      init_row n 6
        (Some { color = Black; role = Man; can_jump = false })
        [];
    ]
  @ [
      init_row n 7
        (Some { color = Black; role = Man; can_jump = false })
        [];
    ]
  @ [ init_row n 8 None [] ]

let game_init n =
  let side_board () = { man_count = 0; lady_count = 0 } in
  {
    board = board_init n;
    w_side_board = side_board ();
    b_side_board = side_board ();
    size = n;
  }

let count_inactive curr_board p_color =
  match p_color with
  | Black ->
      curr_board.b_side_board.lady_count
      + curr_board.b_side_board.man_count
  | White ->
      curr_board.w_side_board.lady_count
      + curr_board.w_side_board.man_count

let square_to_string (square : square) : string =
  match square.occupant with
  | None -> Char.escaped terminal_rep_string.empty
  | Some p -> (
      match p.color with
      | Black ->
          if p.role = Man then Char.escaped terminal_rep_string.black
          else Char.escaped terminal_rep_string.black_dama
      | White ->
          if p.role = Man then Char.escaped terminal_rep_string.white
          else Char.escaped terminal_rep_string.white_dama)

let string_of_row r row =
  List.fold_right
    (fun square acc -> "| " ^ square_to_string square ^ " " ^ acc)
    r
    ("|" ^ string_of_int row)

let rec terminal_rep_string_aux t count =
  match t with
  | [] -> ""
  | h :: t ->
      string_of_row h count ^ "\n"
      ^ terminal_rep_string_aux t (count + 1)

let rec col_label_string count n =
  if count <= n then
    let str =
      char_of_int (int_of_char 'a' - 1 + count) |> Char.escaped
    in
    "  " ^ str ^ " " ^ col_label_string (count + 1) n
  else " \n"

let terminal_rep_string t count =
  col_label_string count t.size ^ terminal_rep_string_aux t.board count

let get_piece (sq : square) =
  let piece = sq.occupant in
  match piece with None -> raise NoPiece | Some piece -> piece

let get_piece_info (sq : square) =
  try
    let p = get_piece sq in
    (p.color, p.role)
  with exn -> raise NoPiece

let get_can_jump (sq : square) =
  try
    let p = get_piece sq in
    p.can_jump
  with exn -> raise NoPiece

let get_side_board t color =
  if color = White then
    (t.w_side_board.lady_count, t.w_side_board.man_count)
  else (t.b_side_board.lady_count, t.b_side_board.man_count)

let update_piece sq (p : piece option) = sq.occupant <- p

let update_can_jump piece boolean = piece.can_jump <- boolean

let remove_pieces captured_sq board =
  let captured_piece = get_piece captured_sq in
  let role = captured_piece.role in
  let p_color = captured_piece.color in
  captured_sq.occupant <- None;
  let white_side = board.w_side_board in
  let black_side = board.b_side_board in
  if p_color = White then
    if role = Lady then
      white_side.lady_count <- white_side.lady_count + 1
    else white_side.man_count <- white_side.man_count + 1
  else if p_color = Black then
    if role = Lady then
      black_side.lady_count <- black_side.lady_count + 1
    else black_side.man_count <- black_side.man_count + 1
  else ()

let rec copy_row = function
  | [] -> []
  | square :: remaining ->
      let s =
        match square.occupant with
        | None -> { square with occupant = None }
        | Some p ->
            {
              square with
              occupant =
                Some
                  {
                    color = p.color;
                    role = p.role;
                    can_jump = p.can_jump;
                  };
            }
      in
      s :: copy_row remaining

let copy_board t =
  let board = t.board in
  let rec copy_board_aux = function
    | [] -> []
    | row :: t -> copy_row row :: copy_board_aux t
  in
  {
    t with
    board = copy_board_aux board;
    w_side_board =
      {
        lady_count = t.w_side_board.lady_count;
        man_count = t.w_side_board.man_count;
      };
    b_side_board =
      {
        lady_count = t.b_side_board.lady_count;
        man_count = t.b_side_board.man_count;
      };
  }

let square_to_json_rep sq : Yojson.Basic.t =
  let c, i = sq.label in
  let label_str = Char.escaped c ^ string_of_int i in
  let occ_str =
    match sq.occupant with
    | None -> ""
    | Some o ->
        let is_can_jump = if o.can_jump then "t" else "f" in
        let piece_str =
          if o.color = Black then if o.role = Lady then "B" else "b"
          else if o.role = Lady then "W"
          else "w"
        in
        piece_str ^ is_can_jump
  in
  `Assoc [ ("label", `String label_str); ("occupant", `String occ_str) ]

let board_to_json t : Yojson.Basic.t =
  let l_of_l =
    List.map (fun x -> List.map square_to_json_rep x) t.board
  in
  let rec board_to_json_aux = function
    | [] -> []
    | h :: t -> [ `List h ] @ board_to_json_aux t
  in
  `List (board_to_json_aux l_of_l)

let sideboard_to_json t : Yojson.Basic.t =
  let w_m = string_of_int t.w_side_board.man_count in
  let w_l = string_of_int t.w_side_board.lady_count in
  let b_m = string_of_int t.b_side_board.man_count in
  let b_l = string_of_int t.b_side_board.lady_count in
  `Assoc
    [
      ("w_lady", `String w_l);
      ("w_men", `String w_m);
      ("b_men", `String b_m);
      ("b_lady", `String b_l);
    ]

let occ_str_to_p occ_str =
  if occ_str <> "" then
    let color =
      if occ_str.[0] = 'B' || occ_str.[0] = 'b' then Black else White
    in
    let can_jump = if occ_str.[1] = 't' then true else false in
    let role =
      if occ_str.[0] = 'B' || occ_str.[0] = 'W' then Lady else Man
    in
    let p = { color; role; can_jump } in
    Some (init_piece_deop p ())
  else None

let rec json_to_sq (sq_lst : Yojson.Basic.t list) =
  match sq_lst with
  | [] -> []
  | `Assoc
      [ ("label", `String label_str); ("occupant", `String occ_str) ]
    :: t ->
      let piece = occ_str_to_p occ_str in
      let c = label_str.[0] in
      let i = int_of_string (Char.escaped label_str.[1]) in
      let label = (c, i) in
      let sq = { occupant = piece; label } in
      sq :: json_to_sq t
  | _ -> failwith "Wrong square format"

let rec json_to_list_sqs (list_sqs : Yojson.Basic.t list) =
  match list_sqs with
  | [] -> []
  | `List list_sq :: t -> json_to_sq list_sq :: json_to_list_sqs t
  | _ -> failwith "Wrong format"

let json_to_board (board : Yojson.Basic.t) =
  match board with
  | `List lists -> json_to_list_sqs lists
  | _ -> failwith "Wrong format"

let init_side_board man lady () = { man_count = man; lady_count = lady }

let json_to_sidebaord (board : Yojson.Basic.t) =
  match board with
  | `Assoc
      [
        ("w_lady", `String w_l);
        ("w_men", `String w_m);
        ("b_men", `String b_m);
        ("b_lady", `String b_l);
      ] ->
      let w_lady = int_of_string w_l in
      let w_men = int_of_string w_m in
      let b_men = int_of_string b_m in
      let b_lady = int_of_string b_l in
      (init_side_board w_men w_lady (), init_side_board b_men b_lady ())
  | _ -> failwith "Wrong sideboard format"

let t_of_board_json
    (board : Yojson.Basic.t)
    (sideboard : Yojson.Basic.t) =
  let board = json_to_board board in
  let w_side_board, b_side_board = json_to_sidebaord sideboard in
  { board; w_side_board; b_side_board; size = 8 }
