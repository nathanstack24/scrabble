open Command
open Board
open State

let rec get_next_command (player_id:int) = 
  print_endline ("Your turn, Player " ^ string_of_int (player_id)) ; (*We'll prob need to pass in players to get_next_command *)
  print_string "> ";
  match read_line () with
  | exception End_of_file -> exit 1
  | text -> 
    try Command.parse text with
    | Command.Malformed -> 
      print_endline "Bad command. Try again"; get_next_command player_id
    | Command.Empty -> get_next_command player_id

let rec execute_command (st:State.t) : State.t =
  let player_id = get_curr_player_id st in 
  match get_next_command player_id with (** the Unit should instead be a player ID *)
  | (Command.Quit) -> exit 0
  | (Command.Place (c,pos)) -> (try (let new_st = State.place_tile c pos st in
                                     print_newline (); 
                                     State.print_board_from_state new_st; 
                                     print_newline(); execute_command new_st)
                                with State.MisplacedTile -> 
                                  print_endline "Tile misplaced"; 
                                  print_newline(); execute_command st)

  | (Command.Remove mybsquare) -> 
    (try let new_st = (State.remove_tile mybsquare st) in 
       print_newline();
       State.print_board_from_state new_st; print_newline(); 
       execute_command new_st
     with 
     |Not_found -> print_endline "No tile at that position"; print_newline(); 
       execute_command st) (*Remove mybsquare from curr_turn. If mybsquare not in curr_turn, *)


  | (Command.Inventory) -> State.print_inventory st; print_newline(); execute_command st
  | (Command.Endturn) -> print_newline(); execute_command (State.end_turn st)

  | (Command.Score) -> let scores = List.assoc player_id (State.get_scores st) in
    print_endline ("Player " ^ string_of_int (player_id) ^ " score: " ^ string_of_int (scores)); 
    print_newline(); execute_command st
(*FOR EACH CASE: At end of patternatching, print out board, ask for next command from current_player. *)


let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to Scrabble!\n");
  execute_command (State.init_state 2); ()

(* Execute the game engine. *)
let () = main ()