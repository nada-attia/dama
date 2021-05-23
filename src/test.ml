open OUnit2
open Board
open Command

(* What we tested: We had 7 modules: Board, Move, Command, State, Ai,
   Gui, Main. Ai, Gui, and Main were tested manually: Ai was tested by
   playing the game against the AI through the terminal as well as the
   GUI. Main was tested through the terminal and Gui was tested through
   the GUI. The tests for the Command module were automated. For the
   Board module, update_piece, remove_pieces, try_upgrade_piece and
   update_can_jump were tested manually through playing the game because
   these functions use mutability and change states. For the State
   module, update_state and game_over were tested manually because
   update_state uses mutability and game_over requires a lot of moves to
   end the game in order to test it. For the Move module,
   where_move_all, get_moveable_squares_reg, get_all_jumps_lady,
   exists_jumps and get_where_jump were tested manually because they
   required a state in the middle of the game to be tested, required
   available jumps, lady pieces, or returned abstract types.
   update_board was also tested manually because of mutability. The
   other functions in Move were automated. For the functions that had
   automated tests, the tests weren't exhaustive because we were limited
   in forming new states and boards by their abstract types, so we used
   automated tests to test basic functionality and used manual testing
   through playing the game with different states to complement this
   automated testing. The tests were developed with glass-box testing.
   We tested copy_state by updating the state several times and making
   copies each time so tests on earlier states still pass. We believe
   that our test suite demonstrates the correctness of our system
   because we tested that the initialization of the board and state is
   correct, and tested many functions with that initialization. If the
   initialization is correct and the functions return the correct value,
   then we believe that further into the game the return value should
   also be correct. We additionally tested exceptions to make sure they
   were being thrown correctly, in addition to making sure errors were
   being handled in the terminal as well as the GUI. We also tested the
   game manually on different edge cases with different states, and it
   works as expected.*)
let print_command = function Move t -> "Move" | Forfeit -> "Forfeit"

let print_label (a1, b1) =
  "(" ^ Char.escaped a1 ^ ", " ^ string_of_int b1 ^ ")"

let print_move_tuple (a, b) =
  "(" ^ print_label a ^ ", " ^ print_label b ^ ")"

let print_label_list label_lst =
  let rec print_labels = function
    | [] -> ""
    | h :: t ->
        print_label h
        ^ (if List.length t <> 0 then ", " else "")
        ^ print_labels t
  in
  "[" ^ print_labels label_lst ^ "]"

let print_piece_tuple (c, r) =
  "("
  ^
  if c = Board.White then "White"
  else "Black" ^ ", " ^ if r = Board.Lady then "Lady" else "Man" ^ ")"

let get_square_test
    (name : string)
    (lbl : char * int)
    (board : Board.t)
    (expected_output : char * int) : test =
  name >:: fun _ ->
  let sq = Move.get_square lbl board in
  assert_equal expected_output (Board.get_label sq) ~printer:print_label

let get_occupant_test
    (name : string)
    (lbl : char * int)
    (board : Board.t)
    (expected_output : Board.color option) : test =
  name >:: fun _ ->
  let sq = Move.get_square lbl board in
  let occupant = Board.get_occupant sq in
  match (occupant, expected_output) with
  | Some piece, Some color -> assert_equal color (Board.get_color piece)
  | None, None -> assert_equal None None
  | _ -> assert_equal true false

let get_sqlst_label_test
    (name : string)
    (sqlst : Board.square list)
    (expected_output : (char * int) list) : test =
  name >:: fun _ ->
  assert_equal expected_output (Board.get_sqlst_label sqlst)

let empty_square_test
    (name : string)
    (lbl : char * int)
    (board : Board.t) : test =
  name >:: fun _ ->
  let sq = Move.get_square lbl board in
  assert_raises Board.NoPiece (fun () -> Board.get_piece_info sq)

let get_square_dir_test
    (name : string)
    (sq : char * int)
    (brd : Board.t)
    (clr : Board.color)
    (dir : Move.direction)
    (expected_output : char * int) : test =
  name >:: fun _ ->
  let sqre = Move.get_square sq brd in
  let sqr = Move.get_square_dir sqre brd clr dir in
  assert_equal [ Move.get_square expected_output brd ] sqr

let get_movable_squares_reg_test
    (name : string)
    (sq : char * int)
    (brd : Board.t)
    (clr : Board.color)
    (expected_output : square list) : test =
  name >:: fun _ ->
  let sqre = Move.get_square sq brd in
  let sqr = Move.get_movable_squares_reg sqre clr brd in
  assert_equal expected_output sqr

let where_move_test
    (name : string)
    (lbl : char * int)
    (board : Board.t)
    (expected_output : square list) : test =
  name >:: fun _ ->
  let sq = Move.get_square lbl board in
  let square_list = Move.where_move board sq in
  assert_equal expected_output square_list

let parse_test
    (name : string)
    (str : string)
    (expected_output : Command.command) : test =
  name >:: fun _ ->
  assert_equal expected_output (Command.parse str)
    ~printer:print_command

let parse_illegalsq_test (name : string) (str : string) : test =
  name >:: fun _ ->
  assert_raises Command.IllegalSquare (fun () -> Command.parse str)

let parse_illegalcom_test (name : string) (str : string) : test =
  name >:: fun _ ->
  assert_raises Command.IllegalCommand (fun () -> Command.parse str)

let parse_nocom_test (name : string) (str : string) : test =
  name >:: fun _ ->
  assert_raises Command.NoCommand (fun () -> Command.parse str)

let parse_move_test
    (name : string)
    (str : string)
    (expected_output : Command.squares_move) : test =
  name >:: fun _ ->
  match Command.parse str with
  | Move t -> assert_equal t expected_output ~printer:print_move_tuple
  | _ -> failwith "Move Failed"

let b = Board.game_init 8

let t1 =
  "| . | . | . | . | . | . | . | . |1\n\
   | w | w | w | w | w | w | w | w |2\n\
   | w | w | w | w | w | w | w | w |3\n\
   | . | . | . | . | . | . | . | . |4\n\
   | . | . | . | . | . | . | . | . |5\n\
   | b | b | b | b | b | b | b | b |6\n\
   | b | b | b | b | b | b | b | b |7\n\
   | . | . | . | . | . | . | . | . |8\n"

let count_inactive_test
    (name : string)
    (expected_output : int)
    (t : Board.t)
    (color : Board.color) =
  name >:: fun _ ->
  assert_equal expected_output
    (Board.count_inactive t color)
    ~printer:string_of_int

let terminal_rep_string_test
    (name : string)
    (t : Board.t)
    (expected_output : string) : test =
  name >:: fun _ ->
  let board = Board.get_board t in
  assert_equal expected_output (Board.terminal_rep_string_aux board 1)

let can_move_test
    (name : string)
    (square : char * int)
    (board : Board.t)
    (turn : Board.color)
    (expected_output : bool) : test =
  name >:: fun _ ->
  let sq = Move.get_square square board in
  let res = Move.can_move sq board turn in
  assert_equal expected_output res

let can_move_all_test
    (name : string)
    (board : Board.t)
    (turn : Board.color)
    (expected_output : bool) : test =
  name >:: fun _ ->
  let res = Move.can_move_all board turn in
  assert_equal expected_output res

let get_all_jumps_test
    (name : string)
    (square : char * int)
    (board : Board.t)
    (turn : Board.color)
    (expected_output : (square * square) list) : test =
  name >:: fun _ ->
  let sq = Move.get_square square board in
  let res = Move.get_all_jumps sq board turn in
  assert_equal expected_output res

let get_other_player_test
    (name : string)
    (color : Board.color)
    (expected_output : Board.color) : test =
  name >:: fun _ ->
  assert_equal expected_output (Board.get_other_player color)

let sqlst = List.hd (Board.get_board b)

let row1 =
  [
    ('a', 1);
    ('b', 1);
    ('c', 1);
    ('d', 1);
    ('e', 1);
    ('f', 1);
    ('g', 1);
    ('h', 1);
  ]

let board_tests =
  [
    count_inactive_test "0 white inactive on initial board" 0 b
      Board.get_init_player;
    terminal_rep_string_test "terminal rep of\n initial board" b t1;
    get_other_player_test "player other than white" White Black;
    get_other_player_test "player other than black" Black White;
    get_occupant_test "occupant of a3 is white" ('a', 3) b (Some White);
    get_occupant_test "occupant of a3 is white" ('a', 1) b None;
    get_occupant_test "occupant of a3 is white" ('h', 8) b None;
    get_occupant_test "occupant of a3 is white" ('f', 6) b (Some Black);
    get_sqlst_label_test "labels of first row" sqlst row1;
  ]

let move_tests =
  [
    get_square_test "square with white piece" ('a', 2) b ('a', 2);
    get_square_test "square with black piece" ('e', 7) b ('e', 7);
    get_square_test "square with black piece" ('a', 4) b ('a', 4);
    get_square_test "b5" ('b', 5) b ('b', 5);
    empty_square_test "square with no piece" ('b', 5) b;
    empty_square_test "square with no piece" ('a', 8) b;
    empty_square_test "square with no piece" ('f', 1) b;
    get_square_dir_test
      "Square ABOVE white piece which has another\n\
      \       white piece on it" ('c', 2) b White Up ('c', 3);
    get_square_dir_test
      "Square below white piece which has another\n\
      \       white piece on it" ('c', 3) b White Down ('c', 2);
    get_square_dir_test
      "Square left of white piece which has another\n\
      \       white piece on it" ('c', 2) b White Left ('b', 2);
    get_square_dir_test
      "Square right of white piece which has\n\
      \       another white piece on it" ('c', 2) b White Right ('d', 2);
    get_square_dir_test
      "Square above black piece which has another\n\
      \       black piece on it" ('b', 7) b Black Up ('b', 6);
    get_square_dir_test
      "Square left of black piece which has another\n\
      \       black piece on it" ('b', 7) b Black Left ('c', 7);
    get_square_dir_test
      "Square right of black piece which has\n\
      \       another black piece on it" ('b', 7) b Black Right ('a', 7);
    get_movable_squares_reg_test "f" ('b', 3) b White
      [ Move.get_square ('b', 4) b ];
    where_move_test "move black piece" ('d', 7) b [];
    where_move_test "move white piece" ('b', 3) b
      [ Move.get_square ('b', 4) b ];
    can_move_test "white piece with nothing in front of it" ('b', 3) b
      White true;
    can_move_test "white piece with something in front of it" ('b', 2) b
      White false;
    can_move_test "black piece with something in front of it" ('b', 7) b
      Black false;
    can_move_test "black piece with nothing in front of it" ('b', 6) b
      Black true;
    get_all_jumps_test "jumps of white piece with no jump" ('b', 3) b
      White [];
    can_move_all_test "white piece has moves in initial state" b White
      true;
    can_move_all_test "black piece has moves in initial state" b Black
      true;
  ]

let command_tests =
  [
    parse_test "parse forfeit" "forfeit" Forfeit;
    parse_test "parse forfeit with extra spaces" "   forfeit    "
      Forfeit;
    parse_move_test "parse move from A6 to A7" "move A6 A7"
      (('a', 6), ('a', 7));
    parse_move_test "parse move from a6 to a7, lowercase" "move a6 a7"
      (('a', 6), ('a', 7));
    parse_move_test "parse move from A6 to A7 with extra spaces"
      "   move A6 A7"
      (('a', 6), ('a', 7));
    parse_illegalsq_test "parse square ER8 (contains two chars)"
      "move ER8 E9";
    parse_illegalsq_test "parse square 67 (contains only numbers)"
      "move 67 E9";
    parse_illegalsq_test "parse square A (contains only letters)"
      "move A E9";
    parse_illegalcom_test "parse move with three squares"
      "move 67 E9 E4";
    parse_illegalcom_test "parse move with no squares" "move ";
    parse_nocom_test "parse empty command" "";
    parse_nocom_test "parse empty command" "   ";
  ]

let player_turn_test
    (name : string)
    (state : State.state)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (State.player_turn state)
    ~printer:(fun x -> x)

let get_turn_test
    (name : string)
    (state : State.state)
    (expected_output : Board.color) : test =
  name >:: fun _ -> assert_equal expected_output (State.get_turn state)

let st = State.init_state b

let b1 = Board.game_init 8

let st1 = State.init_state b1

let st2 = State.update_state st1 (Move (('a', 3), ('a', 4)))

let st3 = State.copy_state st2

let st3 = State.update_state st3 (Move (('b', 6), ('b', 5)))

let t2 =
  "| . | . | . | . | . | . | . | . |1\n\
   | w | w | w | w | w | w | w | w |2\n\
   | . | w | w | w | w | w | w | w |3\n\
   | w | . | . | . | . | . | . | . |4\n\
   | . | b | . | . | . | . | . | . |5\n\
   | b | . | b | b | b | b | b | b |6\n\
   | b | b | b | b | b | b | b | b |7\n\
   | . | . | . | . | . | . | . | . |8\n"

let state_tests =
  [
    player_turn_test "white player is first (as a string)" st "white";
    terminal_rep_string_test "board of initial state"
      (State.get_board st) t1;
    terminal_rep_string_test "board of initial state"
      (State.get_board st3) t2;
    get_turn_test "white player is first" st White;
    get_turn_test "black player is next" st2 Black;
    get_turn_test "white player is next" st3 White;
  ]

let suite =
  "test suite for dama"
  >::: List.flatten
         [ board_tests; move_tests; command_tests; state_tests ]

let _ = run_test_tt_main suite
