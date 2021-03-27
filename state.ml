type current =
  | InProgress
  | Finished
  | Pregame

type state = {
  turn : Board.color;
  board : Board.t;
  current : current;
}

let update_state board command = failwith "Unimplemented"

(* Helper function to check to see if a given square is occupied or not.*)
let check_if_occupied square =
  if square.occupant = None then false else true

let is_valid_label lbl =
  if
    Char.code (Board.label_hd lbl) > 96
    && Char.code (Board.label_hd lbl) < 105
  then if Board.label_tl < 9 && Board.label_hd > 0 then true else false
  else false

(* Helper function [get_vacant_above square board] is the square option
   on top of square [square] from board [board]. If the square on top is
   vacant, it will be returned, otherwise None. *)
let get_vacant_above square board =
  let res_sq = Board.square_above square board in
  if check_if_occupied res_sq then None else Some res_sq

(* Helper function [get_vacant_right square board] is the square option
   to the right of square [square] from board [board]. If the square to
   right is vacant, it will be returned, otherwise None. *)
let get_vacant_right square board =
  let res_sq = Board.square_right square board in
  if check_if_occupied res_sq then None else Some res_sq

(* Helper function [get_vacant_left square board] is the square option
   to the left of square [square] from board [board]. If the square to
   left is vacant, it will be returned, otherwise None. *)
let get_vacant_left square board =
  let res_sq = Board.square_left square board in
  if check_if_occupied res_sq then None else Some res_sq

(* Helper function to get the neighbor squares that can theoretically be
   moved to from a given square noting the color of the piece on said
   square, assuming it is a regular non-Lady piece. *)
let get_movable_squares_reg square color board =
  let squares = [] in
  let squares =
    if get_vacant_above square board <> None then
      get_vacant_above square board :: squares
    else squares
  in
  let squares =
    if get_vacant_right square board <> None then
      get_vacant_right square board :: squares
    else squares
  in
  let squares =
    if get_vacant_left square board <> None then
      get_vacant_left square board :: squares
    else squares
  in
  squares

(* [get_jumps square board color] describes all possible locations the
   occupant of square [square] can jump as a list of squares. (Recall
   that possible jumps are required to be taken in Dama, but 2 jumps
   could be an option.) *)
let get_jumps square board color =
  let label = square.label in
  let ltr_pos = Board.label_hd label in
  let num_pos = Board.label_tl label in
  

(* [where_move board square] describes all of the legal locations the
   occupant of square [square] can move to. *)
let where_move board square = failwith "Unimplemented"

(* [can_move square board st] describes if a piece on square [square]
   can move on board [board] for state [st]. Ensures the square is not
   empty and occupied, that the current occupant has the turn in the
   state, and that there are squares it can legally move to. *)
let can_move square (board : Board.t) (st : state) =
  let condition1 = not (check_if_occupied square) in
  let condition2 = square.occupant.color = state.turn in
  let condition3 = where_move board square <> None in
  condition1 && condition2 && condition3
