open OUnit2
open Board
open Command

(* open Command open State *)

let print_command = function
  | Move t -> "Move"
  | Undo -> "Undo"
  | Forfeit -> "Forfeit"
  | Hint -> "Hint"

let print_tuple ((a1, b1), (a2, b2)) =
  "((" ^ Char.escaped a1 ^ ", " ^ string_of_int b1 ^ "), ("
  ^ Char.escaped a2 ^ ", " ^ string_of_int b2 ^ "))"

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

let parse_move_test
    (name : string)
    (str : string)
    (expected_output : Command.squares_move) : test =
  name >:: fun _ ->
  match Command.parse str with
  | Move t -> assert_equal t expected_output ~printer:print_tuple
  | _ -> failwith "Move Failed"

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
