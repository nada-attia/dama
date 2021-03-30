type current =
  | InProgress
  | Finished
  | Pregame

type state = {
  turn : string;
  board : Board.t;
  current : current;
}

let player_turn (s : state) = "black"

let update_state_move board command = failwith "Unimplemented"

let init_state board = { turn = "black"; board; current = InProgress }
