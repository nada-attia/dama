exception SquareNotFound

type direction =
  | Up
  | Down
  | Left
  | Right

let get_square labl (brd : Board.t) =
  let squares = List.flatten (Board.get_board brd) in
  let rec find_square = function
    | [] -> raise SquareNotFound
    | h :: t -> if Board.get_label h = labl then h else find_square t
  in
  find_square squares

(* Helper function to get list of square left of [square] if exists,
   else []. Behavior varies dependent on color since orientation of
   board changes. Uses list c_val determined by color [color] to perform
   operations. *)
let square_left sq color board =
  let ltr_pos, num_pos = Board.get_label sq in
  let ltr_pos_num = Char.code ltr_pos in
  let c_vals =
    if color = Board.White then [| ltr_pos_num - 1; Char.code 'a'; 1 |]
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
  let ltr_pos, num_pos = Board.get_label sq in
  let ltr_pos_num = Char.code ltr_pos in
  let c_vals =
    if color = Board.White then [| ltr_pos_num + 1; Char.code 'h'; 1 |]
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
  let ltr_pos, num_pos = Board.get_label sq in
  let c_vals =
    if color = Board.White then [| num_pos + 1; 8; 1 |]
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
  let ltr_pos, num_pos = Board.get_label sq in
  let c_vals =
    if color = Board.White then [| num_pos - 1; 1; 1 |]
    else [| 8; num_pos + 1; -1 |]
  in
  if c_vals.(0) >= c_vals.(1) then
    let above1_label = (ltr_pos, num_pos - c_vals.(2)) in
    [ get_square above1_label board ]
  else []

let get_square_dir sq (board : Board.t) (color : Board.color) = function
  | Up -> square_above sq color board
  | Down -> square_below sq color board
  | Left -> square_left sq color board
  | Right -> square_right sq color board

(* Helper function to check to see if a given square is occupied or not.*)
let check_if_occupied square =
  if Board.get_occupant square = None then false else true

(* Helper function to get the neighbor squares that can be moved to from
   a given square noting the color of the piece on said square, assuming
   it is a regular non-Lady piece. *)
let get_movable_squares_reg
    square
    (color : Board.color)
    (board : Board.t) =
  let squares =
    get_square_dir square board color Up
    @ get_square_dir square board color Right
    @ get_square_dir square board color Left
  in
  List.filter (fun square -> Board.get_occupant square = None) squares

(* [get_jumps_dir sq brd clr fun] gets the square to jump to in a given
   direction guided by function [func] for a piece of color [clr] on
   square [sq] on board [brd]*)
let get_jumps_dir sq (brd : Board.t) clr direction =
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
        Board.get_occupant nxt_sq <> None
        && Board.get_occupant nxt_2 = None
        (* get the piece from the piece option *)
      then
        let pc_abv = Option.get (Board.get_occupant nxt_sq) in
        (* ensure that the piece color is enemy color*)
        if Board.get_color pc_abv <> clr then [ (nxt_sq, nxt_2) ]
        else [] (* conditions for jump not met. *)
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
let rec get_all_vac_sq_dir
    acc
    sq
    (clr : Board.color)
    (brd : Board.t)
    direction =
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
  let squares =
    if get_all_vac_sq_dir [] square color board Down <> [] then
      get_all_vac_sq_dir [] square color board Down @ squares
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
    let pc_color, pc_role = Board.get_piece_info sq in
    (* If man piece *)
    if pc_role = Board.Man then
      (* If jump is avalible, it must be taken. But if not... *)
      let jumps = get_all_jumps sq brd pc_color in
      if final_squares jumps = [] then
        get_movable_squares_reg sq pc_color brd
      else final_squares jumps (* If lady piece *)
    else if
      (* If jump is avalible, it must be taken. But if not... *)
      get_all_jumps_lady sq brd pc_color <> []
    then
      let lady_jumps = get_all_jumps_lady sq brd pc_color in
      final_squares lady_jumps
    else get_movable_squares_lady sq pc_color brd
  with _ -> raise Board.EmptyStartSquare

let can_move square board turn =
  let condition1 = check_if_occupied square in
  match Board.get_occupant square with
  | None -> false
  | Some pc ->
      let condition2 = Board.get_color pc = turn in
      let condition3 = where_move board square <> [] in
      condition1 && condition2 && condition3

let update_board turn captured board start_pos end_pos =
  let start_sq = get_square start_pos board in
  let end_sq = get_square end_pos board in
  Board.update_piece end_sq (Board.get_occupant start_sq);
  Board.try_upgrade_piece end_sq board turn;
  Board.update_piece start_sq None;
  match captured with
  | None -> ()
  | Some sq -> Board.remove_pieces sq turn board
