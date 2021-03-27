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

let where_move board square = failwith "Unimplemented"

(* Helper function to check to see if a given square is occupied or not.*)
let check_if_occupied square = if square.occupant = None then false else true

let is_valid_label lbl = 
  if Char.code (Board.label_hd lbl) > 96 and Char.code (Board.label_hd lbl) < 105 then 
    if Board.label_tl < 9 and Board.label_hd > 0 then true 
    else false 
  else false

(* Helper function to get the neighbor squares that can theoretically be moved 
  to from a given square noting the color of the piece on said square, assuming 
  it is a regular non-Lady piece. *)
let get_movable_squares_reg square color board = 
  let labl = square.label in
  let ltr_pos = Board.label_hd labl in
  let num_pos = Board.label_tl labl in 

  let squares = [] in
  let upper_bound = if 8 <= (Board.label_tl labl) + 2 then 8 else (Board.label_tl labl) + 2 in 
  
  (*if white...*)
  let ahead
  let square_ahead1 = Board.get_square labl board

  
  
  for i = (Board.label_tl labl) + 1 to upper_bound do 
    if check_if_occupied (Board.get_square labl board) then () else squares
  done


let can_move (square: Board.square) (board : Board.t) (st : state) = 
  let condition1 = not check_if_occupied in 
  let condition2 = square.occupant.color = state.turn in
  let label = square.label in
  if square.occupant.role = Man then
    List.map get



let can_jump board color = failwith "Unimplemented"

(* helper function to [can_jump] that describes wether or not multiple jumps are 
  avalible from ONE squre (not successive jumps) *)
let can_multi_jump board color = failwith "unimplemented"
