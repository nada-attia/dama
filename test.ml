open OUnit2
open Board
open Command

(* open Command open State *)

let print_command = function
  | Move t -> "Move"
  | Undo -> "Undo"
  | Forfeit -> "Forfeit"
  | Hint -> "Hint"

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
  let sq = Board.get_square lbl board in
  assert_equal expected_output (Board.get_label sq) ~printer:print_label

let empty_square_test
    (name : string)
    (lbl : char * int)
    (board : Board.t) : test =
  name >:: fun _ ->
  let sq = Board.get_square lbl board in
  assert_raises Board.NoPiece (fun () -> Board.get_piece_info sq)

let get_square_dir_test
    (name : string)
    (sq : char * int)
    (brd : Board.t)
    (clr : Board.color)
    (dir : Board.direction)
    (expected_output : char * int) : test =
  name >:: fun _ ->
  let sqre = Board.get_square sq brd in
  let sqr = Board.get_square_dir sqre brd clr dir in
  assert_equal [ Board.get_square expected_output brd ] sqr

let get_movable_squares_reg_test
    (name : string)
    (sq : char * int)
    (brd : Board.t)
    (clr : Board.color)
    (expected_output : square list) : test =
  name >:: fun _ ->
  let sqre = Board.get_square sq brd in
  let sqr = Board.get_movable_squares_reg sqre clr brd in
  assert_equal expected_output sqr

let where_move_test
    (name : string)
    (lbl : char * int)
    (board : Board.t)
    (expected_output : square list) : test =
  name >:: fun _ ->
  let sq = Board.get_square lbl board in
  let square_list = Board.where_move board sq in
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

let parse_move_test
    (name : string)
    (str : string)
    (expected_output : Command.squares_move) : test =
  name >:: fun _ ->
  match Command.parse str with
  | Move t -> assert_equal t expected_output ~printer:print_move_tuple
  | _ -> failwith "Move Failed"

let b = Board.game_init 8

let b1 = Board.game_init 8

let t1 =
  "  a   b   c   d   e   f   g   h   \n\
   |   | . |   | . |   | . |   | . |1\n\
   | w | W | w | W | w | W | w | W |2\n\
   | W | w | W | w | W | w | W | w |3\n\
   |   | . |   | . |   | . |   | . |4\n\
   | . |   | . |   | . |   | . |   |5\n\
   | B | b | B | b | B | b | B | b |6\n\
   | b | B | b | B | b | B | b | B |7\n\
   | . |   | . |   | . |   | . |   |8\n"

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
    (expected_output : string)
    (t : Board.t) : test =
  name >:: fun _ ->
  print_endline (Board.terminal_rep_string t 1);
  print_endline expected_output;
  assert_equal expected_output (Board.terminal_rep_string t 1)

let can_move_test
    (name : string)
    (square : char * int)
    (board : Board.t)
    (turn : Board.color)
    (expected_output : bool) : test =
  name >:: fun _ ->
  let sq = Board.get_square square board in
  let res = Board.can_move sq board turn in
  assert_equal expected_output res

let get_all_jumps_test
    (name : string)
    (square : char * int)
    (board : Board.t)
    (turn : Board.color)
    (expected_output : (square * square) list) : test =
  name >:: fun _ ->
  let sq = Board.get_square square board in
  let res = Board.get_all_jumps sq board turn in
  assert_equal expected_output res

let board_tests =
  [
    count_inactive_test "0 white inactive on initial board" 0 b
      Board.get_init_player;
    (* terminal_rep_string_test "terminal rep of\n initial board" t1 b1; *)
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
      [ Board.get_square ('b', 4) b ];
    where_move_test "move black piece" ('d', 7) b [];
    where_move_test "move white piece" ('b', 3) b
      [ Board.get_square ('b', 4) b ];
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
  ]

let command_tests =
  [
    parse_test "parse undo" "undo" Undo;
    parse_test "parse undo with extra spaces" "    undo   " Undo;
    parse_test "parse hint" "hint" Hint;
    parse_test "parse hint with extra spaces" "   hint   " Hint;
    parse_test "parse forfeit" "forfeit" Forfeit;
    parse_test "parse forfeit with extra spaces" "   forfeit    "
      Forfeit;
    parse_move_test "parse move from A6 to A7" "move A6 A7"
      (('a', 6), ('a', 7));
    parse_move_test "parse move from A6 to A7 with extra spaces"
      "   move A6 A7"
      (('a', 6), ('a', 7));
    parse_illegalsq_test "parse square ER8 (contains two chars)"
      "move ER8 E9";
    parse_illegalsq_test "parse square 67 (contains only numbers)"
      "move 67 E9";
    parse_illegalcom_test "parse move with three squares"
      "move 67 E9 E4";
  ]

let count_inactive_test
    (name : string)
    (expected_output : int)
    (t : Board.t)
    (color : Board.color) =
  name >:: fun _ ->
  assert_equal expected_output
    (count_inactive t color)
    ~printer:string_of_int

let terminal_rep_string_test
    (name : string)
    (expected_output : string)
    (t : Board.t) =
  name >:: fun _ ->
  assert_equal expected_output (terminal_rep_string t 1)

let t1 =
  "  a   b   c   d   e   f   g   h  \n\
   |   | . |   | . |   | . |   | . |1\n\
   | w | W | w | W | w | W | w | W |2\n\
   | W | w | W | w | W | w | W | w |3\n\
   |   | . |   | . |   | . |   | . |4\n\
   | . |   | . |   | . |   | . |   |5\n\
   | B | b | B | b | B | b | B | b |6\n\
   | b | B | b | B | b | B | b | B |7\n\
   | . |   | . |   | . |   | . |   |8\n"

let board_tests =
  [
    count_inactive_test "0 black inactive for initial board" 0
      (Board.game_init 8) (Board.get_color 1);
    count_inactive_test "0 white inactive for initial board" 0
      (Board.game_init 8) (Board.get_color 2);
    terminal_rep_string_test "terminal rep of initial board" t1
      (Board.game_init 8);
  ]

let state_tests = []

let suite =
  "test suite for dama"
  >::: List.flatten [ board_tests; command_tests; state_tests ]

let _ = run_test_tt_main suite
