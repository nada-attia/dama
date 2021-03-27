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

(* Helper function to get the neighbor squares that can theoretically be moved 
  to from a given square noting the color of the piece on said square, assuming 
  it is a regular non-Lady piece. *)
let get_movable_squares_reg square color = 
  let square_pos = square.label in
  let letter_pos = match square_pos with 
  | (h, t) -> h  in 
  let num_pos = match square_pos with 
  | (g,j) -> j in
  let squares = [] in
  (*if white...*)
  for i=num_pos to 8 do 
    if 
  end


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
