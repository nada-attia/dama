open OUnit2
open Board
open Command
open State

let print_command = function
  | Move t -> "Move"
  | Undo -> "Undo"
  | Forfeit -> "Forfeit"
  | Hint -> "Hint"

let parse_test
    (name : string)
    (str : string)
    (expected_output : Command.command) : test =
  name >:: fun _ ->
  assert_equal expected_output (Command.parse str)
    ~printer:print_command

let board_tests = []

let command_tests =
  [
    parse_test "parse undo" "undo" Undo;
    parse_test "parse hint" "hint" Hint;
    parse_test "parse forfeit" "forfeit" Forfeit;
  ]

let state_tests = []

let suite =
  "test suite for A2"
  >::: List.flatten [ board_tests; command_tests; state_tests ]

let _ = run_test_tt_main suite
