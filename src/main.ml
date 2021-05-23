open Constants

let print_error err =
  ANSITerminal.print_string [ ANSITerminal.red ] (err ^ "\n")

let rec next_move state mode =
  let player = State.player_turn state in
  let player_number =
    if player = "white" then "Player 1 " else "Player 2 "
  in
  let player_and_number = player_number ^ "(" ^ player ^ ")" in
  if State.game_over state then
    print_endline
      ("The game is over." ^ player_and_number ^ " is the winner!")
  else
    print_string
      ("\n" ^ Board.terminal_rep_string (State.get_board state) 1 ^ "\n");
  print_endline player_and_number;
  print_string "> ";
  (* && true for ai mode, && false for multiplayer mode*)
  if player = "black" && mode then
    let new_state =
      Ai.ai_next_move (State.get_turn state) state ai_level
    in
    next_move new_state mode
  else
    match read_line () with
    | exception End_of_file -> ()
    | command -> (
        match State.update_state state (Command.parse command) true with
        | exception Command.IllegalCommand ->
            print_error
              "The squares you are moving to and from must be in the \
               form of a valid letter followed by a valid number";
            next_move state mode
        | exception Command.IllegalSquare ->
            print_error
              "Please enter squares in the form of a letter followed \
               by a number, in range";
            next_move state mode
        | exception Command.NoCommand ->
            print_error "Please enter a move";
            next_move state mode
        | exception State.IllegalMove ->
            print_error "Illegal move";
            next_move state mode
        | exception Board.EmptyStartSquare ->
            print_error "The start square cannot be empty";
            next_move state mode
        | exception Board.NoPiece ->
            print_error "There is no piece on the specified square";
            next_move state mode
        | exception Move.SquareNotFound ->
            print_error "Square not found";
            next_move state mode
        | exception State.RequiredJumpNotTaken ->
            print_error "Required jump not taken";
            next_move state mode
        | new_state -> next_move new_state mode)

(** [play_game] starts the game. *)
let play_game board mode =
  let state = State.init_state board in
  next_move state mode

let rec mode () =
  print_endline "\nType 1 for AI mode or 2 for Multiplayer mode";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | i -> (
      let board = Board.game_init 8 in
      match i with
      | "1" -> play_game board true
      | "2" -> play_game board false
      | _ ->
          print_error "invalid mode";
          mode ())

(* init board prompt player who's turn it is to play (black starts)
   parse string and convert the exceptions into readable error messages
   check move legality and convert exceptions move and update state with
   other player's turn *)

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\n\nWelcome to Dama (Turkish Draughts).\n";
  mode ()

(* Execute the game engine. *)
let () = main ()
