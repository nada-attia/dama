open OUnit2
open Board

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

let parse_move_test
    (name : string)
    (str : string)
    (expected_output : Command.squares_move) : test =
  name >:: fun _ ->
  match Command.parse str with
  | Move t -> assert_equal t expected_output ~printer:print_tuple
  | _ -> failwith "Move Failed"

let board_tests = []

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
  ]

let state_tests = []

let suite =
  "test suite for dama"
  >::: List.flatten [ board_tests; command_tests; state_tests ]

let _ = run_test_tt_main suite
