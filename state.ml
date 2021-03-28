type current =
  | InProgress
  | Finished
  | Pregame

type state = {
  turn : Board.color;
  board : Board.t;
  current : current;
}

let get_turn = failwith "Unimplemented"

let update_state_move board command = failwith "Unimplemented"
