type square_label = char * int

type squares_move = square_label * square_label

exception IllegalCommand

exception IllegalSquare

exception NoCommand

type command =
  | Move of squares_move
  | Undo
  | Forfeit
  | Hint

(* [string_to_tuple s] converts a list of strings in the form [["a",
   "9"]] to [("a", 9)]

   If the first element is not one lowercase letter and the second
   element is not a string that contains only an int (and no characters)
   an IllegalSquare is raised *)
let string_to_tuple s =
  if String.length s < 2 then raise IllegalSquare
  else
    let nums = String.sub s 1 (String.length s - 1) in
    let first_char_code = Char.code s.[0] in
    try
      let a = Char.code 'a' in
      let h = Char.code 'h' in
      if first_char_code < a || first_char_code > h then
        raise IllegalSquare
      else (s.[0], int_of_string nums)
    with exn -> raise IllegalSquare

let get_label (start_pos, end_pos) =
  let start_label = string_to_tuple start_pos in
  let end_label = string_to_tuple end_pos in
  (start_label, end_label)

(* [get_positions lst] converts a list containing two elements into a
   tuple. if the list has more or less than two elements, an
   IllegalCommand exception will be thrown *)
let get_positions = function
  | start_pos :: end_pos :: t ->
      let s_pos_lower = String.lowercase_ascii start_pos in
      let e_pos_lower = String.lowercase_ascii end_pos in
      if List.length t = 0 then get_label (s_pos_lower, e_pos_lower)
      else raise IllegalCommand
  | _ -> raise IllegalCommand

let return_command action remaining_command =
  if action = "move" && List.length remaining_command = 2 then
    Move (get_positions remaining_command)
  else if action = "forfeit" && List.length remaining_command = 0 then
    Forfeit
  else if action = "hint" && List.length remaining_command = 0 then Hint
  else if action = "undo" && List.length remaining_command = 0 then Undo
  else raise IllegalCommand

let parse s =
  let trimmed = String.trim s in
  let split_lst = String.split_on_char ' ' trimmed in
  let s_lst = if String.length trimmed != 0 then split_lst else [] in
  let s_lst_clean = List.filter (fun x -> x <> "") s_lst in
  match s_lst_clean with
  | [] -> raise NoCommand
  | h :: t ->
      let action = String.trim h in
      return_command action t
