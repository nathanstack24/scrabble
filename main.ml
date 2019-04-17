open Command
open State

let rec get_next_command player : Command.command = 
  print_endline "Your turn, [PLAYERNAME]"; (*We'll prob need to pass in players to get_next_command *)
  print_string "> ";
  match read_line () with
  | exception End_of_file -> exit 1
  | text -> 
    try Command.parse text with
    | Command.Malformed -> 
      print_endline "Bad command. Try again"; get_next_command player
    | Command.Empty -> get_next_command player

let rec execute_command (st:State.t) : State.t =
  let p = (State.get_curr_turn st) in 
  match (get_next_command ()) with (** the Unit should instead be a player ID *)
  | (Command.Quit) -> exit 0
  | (Command.Place mybsquare) -> failwith "whaaat"
  | (Command.Remove mybsquare) -> print_endline "Yo it ain't there"; failwith "unimplemented" (*Remove mybsquare from curr_turn. If mybsquare not in curr_turn, *)
  | (Command.Endturn) -> failwith "Unimplemnted" (*Check validity of word in curr_turn, if word bad then restart turn. If good, then
                                                   set current_player <= next_player. *)
  | (Command.Score) -> failwith "Unimplmented"(*Print scores like this:
                                                Player1: 10
                                                Player2: 4
                                                Player3: 97*)
(*FOR EACH CASE: At end of patternatching, print out board, ask for next command from current_player. *)


let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to Scrabble!\n");
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | _ -> failwith "unimplemented"

(* Execute the game engine. *)
let () = main ()