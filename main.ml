
let rec get_next_command c : Command.command = 
  print_endline "Your turn, [PLAYERNAME]"; (*We'll prob need to pass in players to get_next_command *)
  print_string "> ";
  match read_line () with
  | exception End_of_file -> exit 1
  | text -> 
    try Command.parse text with
    | Command.Malformed -> 
      print_endline "Bad command. Try again"; get_next_command ()
    | Command.Empty -> get_next_command ()

let rec execute_command (me:State.t) = failwith "Unimplemented"


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