open Command
open Board
open State

let rec get_next_command (player_id:int) (st:State.t)= 
  print_endline ("Your turn, Player " ^ string_of_int (player_id)) ;
  let score = List.assoc player_id (State.get_scores st) in
  print_endline ("Your score is: " ^ (string_of_int score));
  print_string ("Your letters are: " ); State.print_inventory st; print_newline();
  print_string "> ";
  match read_line () with
  | exception End_of_file -> exit 1
  | text -> 
    try Command.parse text with
    | Command.Malformed -> 
      print_endline "Bad command. Try again"; get_next_command player_id st
    | Command.Empty -> get_next_command player_id st 
    | Failure x -> 
      print_endline "Bad command. Enter 'help' for a list of valid commands"; 
      get_next_command player_id st

let rec execute_command (st:State.t) : State.t =
  let player_id = get_curr_player_id st in 
  match get_next_command player_id st with 
  | (Command.Quit) -> exit 0
  | (Command.Place (c,pos)) -> (try (let new_st = State.place_tile c pos st in
                                     print_newline(); 
                                     State.print_board_from_state new_st; 
                                     print_newline(); execute_command new_st)
                                with 
                                | State.MisplacedTile -> 
                                  print_newline();
                                  print_endline "The tile was misplaced, try again"; 
                                  print_newline(); State.print_board_from_state st; 
                                  print_newline(); execute_command st
                                | State.NotInInv ->
                                  ANSITerminal.print_string [ANSITerminal.red] 
                                    "That letter is not in your inventory. Try again.";
                                  print_newline();
                                  execute_command st;
                                | State.Occupied -> 
                                  ANSITerminal.print_string [ANSITerminal.red] 
                                    "That board square is already occupied. Try again.";
                                  print_newline();
                                  execute_command st;)


  | (Command.Remove mybsquare) -> 
    (try let new_st = (State.remove_tile mybsquare st) in 
       print_newline(); State.print_board_from_state new_st; print_newline();
       execute_command new_st
     with 
     |Not_found -> print_newline(); 
       print_endline "No tile to be removed at that position"; 
       print_newline(); State.print_board_from_state st; print_newline(); 
       execute_command st) 

  | (Command.Inventory) -> print_string ("Your letters are: " ); 
    State.print_inventory st; print_newline (); execute_command st
  | (Command.Endturn) -> 
    (try let new_st = State.end_turn st in 
       print_newline(); State.print_board_from_state new_st; 
       print_newline(); ANSITerminal.(print_string [green] "Great turn! \n"); 
       execute_command (new_st)
     with 
     |Board.BadWord -> print_newline(); State.print_board_from_state st; 
       print_newline(); ANSITerminal.(print_string [red]
                                        ("Some words on the board are not "^
                                         "valid. Try again. \n"));
       execute_command st
     |Board.NotConnected -> 
       print_newline(); State.print_board_from_state st; 
       print_newline(); 
       ANSITerminal.(print_string [red]
                       ("Some tiles on the board are not connected to the " ^ 
                        "center tile. Try again \n")); 
       execute_command st
     | State.EndGame -> print_newline(); 
       ANSITerminal.(print_string [green]
                       "The Game is Over! Here are the final scores: \n\n");
       State.print_scores st;

       exit 0)

  | (Command.Score) -> State.print_scores st; execute_command st

  | (Command.Help) ->   print_newline(); ANSITerminal.(print_string [green]
                                                         "The commands are 
      place [Char] [x_pos] [y_pos] to place tile [Char] at ([pos_x], [pos_y])
      remove [x_pos] [y_pos] to remove the tile at ([pos_x], [pos_y])
      inventory to check the letters in your inventory
      score to print all players' scores
      board to print out the current board
      endturn to end your turn; if your moves are not valid, your turn will 
      not end until you play a valid word.
      help to print this message again
      quit to quit\n \n"); 
    print_newline(); execute_command st

  |Command.Board -> print_newline(); State.print_board_from_state st;
    print_newline(); execute_command st

let initial_commands = 0

let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to Scrabble!\n\n");
  ANSITerminal.(print_string [blue] "The game currently has 2 Players
For a turn to be valid, all words placed must be in the Scrabble dictionary
and all tiles must be connected to the center (8,8) \n\n");
  ANSITerminal.(print_string [green]
                  "The commands are 
        place [Char] [x_pos] [y_pos]
        remove [x_pos] [y_pos]
        inventory 
        score
        board
        endturn
        help
        quit \n \n");
  ANSITerminal.(print_string [red] "The board:");
  print_newline(); State.print_board_from_state (State.init_state 2); 
  print_newline();
  (* ANSITerminal.(print_string [green]
                  "\n\nHow many players?\n\n");
     match read_line () with
     | exception End_of_file -> exit 1
     | text ->  *)
  execute_command (State.init_state 2); ()

(* Execute the game engine. *)
let () = main ()