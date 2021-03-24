open OUnit2
open Board
open Command
open State

let board_tests = []
let command_tests = []
let state_tests = []


let suite =
  "test suite for A2"
  >::: List.flatten [ board_tests; command_tests; state_tests ]

let _ = run_test_tt_main suite