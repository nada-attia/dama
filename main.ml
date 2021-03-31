let print_error err =
  ANSITerminal.print_string [ ANSITerminal.red ] (err ^ "\n")

let rec next_move state =
  if State.game_over state then print_endline "The game is over"
  else
    let player = State.player_turn state in
    let player_number =
      if player = "white" then "Player 1 " else "Player 2 "
    in
    print_endline (player_number ^ "(" ^ player ^ "):");
    print_string "> ";
    match read_line () with
    | exception End_of_file -> ()
    | command -> (
        match State.update_state state (Command.parse command) with
        | exception Command.IllegalCommand ->
            print_error
              "The squares you are moving to and from must be in the \
               form of a valid letter followed by a valid number";
            next_move state
        | exception Command.IllegalSquare ->
            print_error
              "Please enter squares in the form of a letter followed \
               by a number, in range";
            next_move state
        | exception Command.NoCommand ->
            print_error "Please enter a move";
            next_move state
        | exception State.IllegalMove ->
            print_error "Illegal move";
            next_move state
        | new_state -> next_move new_state)

(** [play_game] starts the game. *)
let play_game board =
  let state = State.init_state board in
  next_move state

(* init board prompt player who's turn it is to play (black starts)
   parse string and convert the exceptions into readable error messages
   check move legality and convert exceptions move and update state with
   other player's turn *)

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\n\nWelcome to Dama (Turkish Draughts).\n";
  let board = Board.game_init 8 in
  print_string (Board.terminal_rep_string board 1);
  play_game board

(* Execute the game engine. *)
let () = main ()
