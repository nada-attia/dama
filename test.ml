open OUnit2
open Board
open Command

(* open Command open State *)

let print_command = function
  | Move t -> "Move"
  | Undo -> "Undo"
  | Forfeit -> "Forfeit"
  | Hint -> "Hint"

let print_move_tuple ((a1, b1), (a2, b2)) =
  "((" ^ Char.escaped a1 ^ ", " ^ string_of_int b1 ^ "), ("
  ^ Char.escaped a2 ^ ", " ^ string_of_int b2 ^ "))"

let print_piece_tuple (c, r) =
  "("
  ^
  if c = Board.White then "White"
  else "Black" ^ ", " ^ if r = Board.Lady then "Lady" else "Man" ^ ")"

let get_square_test
    (name : string)
    (lbl : char * int)
    (board : Board.t)
    (expected_output : Board.color * Board.role) : test =
  name >:: fun _ ->
  let sq = Board.get_square lbl board in
  assert_equal expected_output
    (Board.get_piece_info sq)
    ~printer:print_piece_tuple

let empty_square_test
    (name : string)
    (lbl : char * int)
    (board : Board.t) : test =
  name >:: fun _ ->
  let sq = Board.get_square lbl board in
  assert_raises Board.NoPiece (fun () -> Board.get_piece_info sq)

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

let board_tests =
  [
    get_square_test "square with white piece" ('a', 2) b
      (Board.White, Board.Man);
    get_square_test "square with black piece" ('e', 7) b
      (Board.Black, Board.Man);
    empty_square_test "square with no piece" ('b', 5) b;
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

let state_tests = []

let suite =
  "test suite for dama"
  >::: List.flatten [ board_tests; command_tests; state_tests ]

let _ = run_test_tt_main suite
