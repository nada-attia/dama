type t = Node of (State.state list * (State.state * float) * t list)

(** [evaluate old_state new_state] that has a higher value is better for
    Black and lower value is better for white*)
let evaluate old_t new_t =
  let inactive_old = Board.count_inactive old_t Board.Black in
  let inactive_new = Board.count_inactive new_t Board.Black in
  if inactive_new > inactive_old then -1.0
  else if inactive_new < inactive_old then 1.0
  else 0.0

(** [end_moves_helper state square lst] is the list of states succeeding
    [state] moving from [square] to squares in [lst]*)
let rec end_moves_helper state square = function
  | [] -> []
  | h :: t ->
      let copy_state = State.copy_state state in
      (* let c, i = Board.get_label h in print_endline (Char.escaped c ^
         string_of_int i); *)
      print_endline
        (Board.terminal_rep_string (State.get_board state) 1);
      State.update_state copy_state
        (Command.Move (Board.get_label square, Board.get_label h))
      :: end_moves_helper state square t

(** [make_states_level state] creates one level of states from [state]
    representing all possible states*)
let make_states_level (state : State.state) =
  print_endline "start";
  let where_move_all =
    Move.where_move_all (State.get_board state) (State.get_turn state)
  in
  print_endline "end";
  let rec make_states_level_aux = function
    | [] -> []
    | (square, end_moves) :: remaining ->
        (* let c, i = Board.get_label square in print_endline
           (Char.escaped c ^ string_of_int i); *)
        end_moves_helper state square end_moves
        @ make_states_level_aux remaining
  in
  make_states_level_aux where_move_all

(** *)
let rec initialize_tree
    (state : State.state)
    n
    (path_acc : State.state list)
    (prev_eval : float) : t list =
  let child_states = make_states_level state in
  let rec initialize_tree_aux = function
    | [] -> []
    | h :: t ->
        let evaluate =
          (evaluate (State.get_board state) (State.get_board h)
          +. prev_eval)
          /. 2.0
        in
        Node
          ( state :: path_acc,
            (h, evaluate),
            if n > 0 then
              initialize_tree h (n - 1) (state :: path_acc) evaluate
            else [] )
        :: initialize_tree_aux t
  in
  initialize_tree_aux child_states

(* let tree state n = Node ([], (state, 0.0), initilize_tree state n []
   0.0) *)

(** [evaluate parent n sign op] is the best path from [parent] [n]
    levels deep. [op] is ( > ) and [sign] is -1 if the ai is black, and
    vice versa if the ai is white *)
let evaluate_tree state n sign op =
  let children = initialize_tree state n [] 0.0 in
  let rec evaluate_tree_aux best = function
    | [] -> best
    | Node (p, (state, eval), ch) :: t ->
        let k, best_path = best in
        if List.length p = n then
          if op eval k then evaluate_tree_aux (eval, p) t
          else evaluate_tree_aux best t
        else evaluate_tree_aux best t
  in
  evaluate_tree_aux (sign *. 2., []) children

let ai_next_move state n =
  let best_val, path = evaluate_tree state n (-1.) ( > ) in
  if path <> [] then path |> List.rev |> List.hd else failwith "no path"
