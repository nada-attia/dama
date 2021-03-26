type squares_move = string * string

exception IllegalCommand

exception NoCommand

type command =
  | Move of squares_move
  | Undo
  | Forfeit
  | Hint

(* [get_positions lst] converts a list containing two elements into a
   tuple. if the list has more or less than two elements, an
   IllegalCommand exception will be thrown *)
let get_positions = function
  | start_pos :: end_pos :: t ->
      if List.length t = 0 then (start_pos, end_pos)
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
  match s_lst with
  | [] -> raise NoCommand
  | h :: t ->
      let action = String.trim h in
      return_command action t
