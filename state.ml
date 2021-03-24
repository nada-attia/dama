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

let can_move board color = failwith "Unimplemented"

let can_jump board color = failwith "Unimplemented"
