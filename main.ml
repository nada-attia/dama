let print_error err =
  ANSITerminal.print_string [ ANSITerminal.red ] (err ^ "\n")

let rec next_move state =
  let player = State.player_turn state in
  let player_number =
    if player = "black" then "Player 1 " else "Player 2 "
  in
  print_endline (player_number ^ "(" ^ player ^ "):");
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | command -> (
      match Command.parse command with
      | exception Command.IllegalCommand ->
          print_error
            "The squares you are moving to and from must be in the \
             form of a valid letter followed by a valid number";
          next_move state
      | exception Command.IllegalSquare -> ()
      | exception Command.NoCommand -> ()
      | Move command -> ()
      | Forfeit -> ()
      | Undo -> ()
      | Hint -> ())

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
  print_endline "Please enter the name of player 1 (black).\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | command -> (
      ();
      print_endline "Please enter the name of player 2 (white).\n";
      print_string "> ";
      match read_line () with
      | exception End_of_file -> ()
      | command ->
          let board = Board.game_init 8 in
          print_string (Board.terminal_rep_string board 1);
          play_game board)

(* Execute the game engine. *)
let () = main ()
