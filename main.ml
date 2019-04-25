open Command
open Board
open State


(** [print_word_list word_lst] returns the string of  all the words in a 
    [word_lst], separated by commas*)
let rec word_list_to_string word_lst = 
  match word_lst with 
  |[] -> ""
  |h::[] -> h
  |h::t -> h ^ ", " ^ (word_list_to_string t)

let erase_above = fun () ->
  ANSITerminal.erase Above;
  ANSITerminal.set_cursor 1 2

let default = Unix.tcgetattr Unix.stdin

let set_canonical on =
  let attr = Unix.tcgetattr Unix.stdin in
  let attr' = { attr with c_icanon = on } in
  Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH attr'

let rec detect_cursor_changes = fun () ->
  set_canonical false;
  match Pervasives.input_char stdin with
  | 'a' -> Left
  | 'd' -> Right
  | 'w' -> Up
  | 's' -> Down
  | '\n' -> raise NoCursorChange
  | _ -> Invalid

let help_message = 
  "To move the cursor (denoted by yellow background and 
    blinking foreground), use the 'A','S','D', and 'W' keys, which
    correspond to moving the cursor left, down, right, and up, 
    respectively. Once you have chosen a position, press the 
    Return key. 

    Then, type the letter corresponding to the tile
    that you'd like to play at the chosen position and press the
    Return key to play the tile at that position.

    If the chosen position is currently occupied, type 'remove' and then press
    the return key to remove the tile at that position.

    Other commands:

    score (prints the current score of all players in the game)
    endturn (ends the current player's turn once they have formed the desired word(s))
    skip (skips the current player's turn)
    help (prints this message to the console)
    quit (quits the game) \n \n"


let rec get_next_command (player_id:int) (st:State.t) = 

  print_endline ("Your turn, Player " ^ string_of_int (player_id)) ;
  let score = List.assoc player_id (State.get_scores st) in
  print_endline ("Your score is: " ^ (string_of_int score));
  print_string ("Your letters are: " ); State.print_inventory st; print_newline();
  print_string "> "; flush stdout;
  try 
    (let change = detect_cursor_changes () in
     set_canonical true;
     if change=Invalid then (erase_above ();
                             State.print_board_from_state st; 
                             print_newline(); get_next_command player_id st) else
       let new_st = new_state_with_cursor_change st change in 
       erase_above ();
       State.print_board_from_state new_st; 
       print_newline(); get_next_command player_id new_st)
  with NoCursorChange ->
    (Unix.tcsetattr Unix.stdin Unix.TCSANOW default;
     match read_line () with
     | exception End_of_file -> exit 1
     | text -> 
       try Command.parse text st with
       | Board.InvalidChar -> print_endline ("Bad command. You can only place " ^ 
                                             "letters that are currently in your " ^
                                             "inventory. Try again.");
         flush stdout;
         Unix.sleep 1;
         erase_above ();
         State.print_board_from_state st; 
         print_newline(); get_next_command player_id st
       | Command.Empty 
       | Command.Malformed 
       | Failure _ ->
         print_endline "Bad command. Enter 'help' for a list of valid commands";
         flush stdout;
         Unix.sleep 1;
         erase_above ();
         State.print_board_from_state st; 
         print_newline(); get_next_command player_id st)



let rec execute_command (st:State.t) : State.t =
  let player_id = get_curr_player_id st in 
  if is_player_bot player_id st then ( 
    print_endline ("Player " ^ string_of_int (player_id) ^ "'s turn (Bot)") ;
    let new_st = State.end_turn (State.perfect_turn st) in
    let new_words = State.get_state_word_diff st new_st in
    let points = State.get_state_score_diff st new_st in
    erase_above();
    State.print_board_from_state new_st; 
    print_newline(); 

    let new_score = get_score new_st player_id in 
    ANSITerminal.(print_string [green] 

                    ("Player " ^ string_of_int (player_id) 
                     ^ " made the following word(s): " ^ (word_list_to_string new_words) ^ "\n" ^"Earning " ^ (string_of_int points) ^ " points \n"^
                     "Player "^ (string_of_int player_id) ^ 
                     "'s score is now " ^ 
                     (string_of_int new_score)) );  

    print_newline(); execute_command (new_st)
  )
  else (
    match get_next_command player_id st with 
    | (Command.Quit) -> exit 0
    | (Command.Place (c,pos)) -> 
      (try (let new_st = State.place_tile c pos st in
            erase_above ();
            State.print_board_from_state new_st; 
            print_newline(); execute_command new_st)
       with 
       | State.MisplacedTile -> 
         print_newline();
         ANSITerminal.print_string [ANSITerminal.red] 
           "The tile was misplaced, try again";
         flush stdout;
         Unix.sleep 1;
         erase_above ();
         print_newline(); State.print_board_from_state st; 
         print_newline(); execute_command st
       | State.NotInInv ->
         ANSITerminal.print_string [ANSITerminal.red] 
           "That letter is not in your inventory. Try again.";
         print_newline();
         flush stdout;
         Unix.sleep 1;
         erase_above ();
         State.print_board_from_state st; 
         print_newline(); execute_command st;
       | Board.InvalidPos pos -> 
         ANSITerminal.print_string [ANSITerminal.red]
           ("(" ^ (string_of_int (get_x_pos pos)) ^ ", " ^
            (string_of_int (get_y_pos pos)) ^ ") is not"^
            "a valid position. Try again.");
         print_newline();
         flush stdout;
         Unix.sleep 1;
         erase_above ();
         State.print_board_from_state st; 
         print_newline(); execute_command st;

       | State.Occupied -> 
         ANSITerminal.print_string [ANSITerminal.red] 
           "That board square is already occupied. Try again.";
         flush stdout;
         Unix.sleep 1;
         erase_above ();
         State.print_board_from_state st; 
         print_newline(); execute_command st;)


    | (Command.Remove mybsquare) -> 
      (try let new_st = (State.remove_tile mybsquare st) in 
         erase_above(); State.print_board_from_state new_st; print_newline();
         execute_command new_st
       with 
       |Not_found -> print_newline(); 
         print_endline "No tile to be removed at that position"; 
         flush stdout;
         Unix.sleep 1;
         erase_above(); State.print_board_from_state st; print_newline(); 
         execute_command st) 
    | (Command.Endturn) -> 
      (try let new_st = State.end_turn st in 
         let new_words = State.get_state_word_diff st new_st in
         let points = State.get_state_score_diff st new_st in
         erase_above();
         State.print_board_from_state new_st; 
         print_newline(); 
         let player = get_curr_player_id st in 
         let new_score = get_score new_st player in 
         ANSITerminal.(print_string [green] 
                         ("Great turn!
You made the following word(s): " ^ (word_list_to_string new_words) ^ "\n" ^
                          "You earned " ^ (string_of_int points) ^ " points \n"^
                          "Player "^ (string_of_int player) ^ 
                          "'s score is now " ^ 
                          (string_of_int new_score));
                       flush stdout;
                       Unix.sleep 2;

                       print_newline(); execute_command (new_st))
       with 
       |Board.BadWord ->  ANSITerminal.(print_string [red]
                                          ("Some words on the board are not "^
                                           "valid. Try again. \n"));
         flush stdout;
         Unix.sleep 1;
         erase_above ();
         State.print_board_from_state st; 
         print_newline(); execute_command st
       |Board.NotConnected -> 
         print_newline(); 
         ANSITerminal.(print_string [red]
                         ("Some tiles on the board are not connected to the " ^ 
                          "center tile. Try again \n")); 
         flush stdout;
         Unix.sleep 1;
         erase_above ();
         State.print_board_from_state st; 
         print_newline(); execute_command st
       |Board.OneLetter ->
         print_newline(); 
         ANSITerminal.(print_string [red]
                         ("Words placed must be at least 2 letters long. " ^ 
                          "Try again \n")); 
         flush stdout;
         Unix.sleep 1;
         erase_above ();
         State.print_board_from_state st; 
         print_newline(); execute_command st

       | State.EndGame -> print_newline(); 
         ANSITerminal.(print_string [green]
                         "The Game is Over! Here are the final scores: \n\n");
         State.print_scores st;

         exit 0)

    | (Command.Score) -> 
      State.print_scores st;
      flush stdout;
      Unix.sleepf 1.5; 
      erase_above ();
      State.print_board_from_state st; 
      print_newline(); execute_command st

    | (Command.Help) ->   print_newline(); 
      ANSITerminal.(print_string [green] help_message); 
      flush stdout;
      Unix.sleep 5;
      erase_above ();
      State.print_board_from_state st; 
      print_newline(); execute_command st

    | (Command.Skip) -> 
      (try (let new_st = State.skip_curr_turn st in 
            erase_above ();
            State.print_board_from_state new_st; 
            print_newline(); execute_command new_st)
       with 
       | CannotSkip -> 
         ANSITerminal.(print_string [red]
                         ("You cannot skip your turn after placing tiles on the"
                          ^"board. Remove all placed tiles and then try again. \n")); 
         flush stdout;
         Unix.sleep 2;
         erase_above ();
         State.print_board_from_state st; 
         print_newline(); execute_command st)

    |Command.Perfect -> let new_st = State.perfect_turn st in 
      print_newline(); State.print_board_from_state new_st;
      print_newline(); execute_command (new_st))


(** [parse_num_players str] returns the integer *)
let parse_num_players (str: string) = 
  let cmd = (clean_str (String.split_on_char ' ' (String.trim str))) in
  if List.length cmd = 0 then raise Empty else 
    match cmd with 
    | n::[] -> (try (let i = (int_of_string n) in (i)) with | _ -> raise Malformed)
    | _ -> raise Malformed

(** [get_integer low upper] prompts the user to input an integer which is only
    returned if it is between [low] and [upper] (non-inclusive)*)
let rec get_integer low upper = match read_line () with
  | exception End_of_file -> exit 1
  | txt -> try 
      (match parse_num_players txt with
       |num when num < upper && num > low -> num
       |_ -> ANSITerminal.print_string [ANSITerminal.red] 
               ("You must enter an integer between 1 and 4. Try again"); 
         get_integer low upper)
    with 
    | _ -> ANSITerminal.print_string [ANSITerminal.red] 
             ("You must enter an integer between 1 and 4. Try again"); 
      get_integer low upper

let main () =
  (*ANSITerminal.erase Above;
    ANSITerminal.set_cursor 1 1;
    ANSITerminal.(print_string [black;Blink]
                  "
     ██╗    ██╗███████╗██╗      ██████╗ ██████╗ ███╗   ███╗███████╗
     ██║    ██║██╔════╝██║     ██╔════╝██╔═══██╗████╗ ████║██╔════╝
     ██║ █╗ ██║█████����������������  ██║     ██║     ██║   ██║██╔████╔██║█████╗  
     ██║███╗██║██╔══╝  ██║     ██║     ██║   ██║██║╚██╔╝██║██╔══╝  
     �����███╔███╔╝████���██╗███████╗██████╗╚██████╔╝██║ ��═╝ ██║███████╗
     ╚══╝╚══╝ ╚══════╝╚══════╝ ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚══════╝ 
     \n");
    flush stdout;
    Unix.sleepf 1.5;
    ANSITerminal.(print_string [red] "
     ████████╗ ██████╗ 
     ╚��═██╔══╝██╔══��██╗
     ��█║   ██║   ██║
     ██║   ██║   ██║
     ██║   ╚██████╔╝
     ╚═╝    ╚═════╝ 
     \n");
    flush stdout;
    Unix.sleepf 1.5;
    ANSITerminal.(print_string [black] " 
     ███████╗ ██████╗██████╗  █████╗ ██████╗ ██████╗ ██╗     ███████╗
     ██╔════╝██╔════╝██╔══██╗██╔══██╗██╔══██╗██╔══██╗██║     ██╔════╝
     ███████╗██║     ██████╔╝███████║██████╔╝██████╔╝██║     █████╗  
     ╚════██║██║     ██╔══██╗██╔══██║██╔══██╗██╔══██╗██║     ██╔══╝  
     ██████║╚██████╗█��║  ██��██║  ██║██████╔╝████ █╔=█  ██╗█████��█╗
                                                                                                                       ══==═╝ ╚════=╝  ╚═╝╚═╝  ╚═╝╚═════╝ ╚═════╝ ╚══════╝╚══════╝
                                                                                                                                             \n");
    flush stdout;
    Unix.sleepf 1.5;*)
  ANSITerminal.erase Above;
  ANSITerminal.set_cursor 1 1;
  Unix.sleepf 1.5;

  ANSITerminal.(print_string [blue] ("\n\nEnter total number of players (1-4): ") );
  let player_num = get_integer 0 5 in
  ANSITerminal.(print_string [blue] ("\n\nOf these, how many bots? (0-"
                                     ^string_of_int (player_num-1)^ "): ") );
  let npc_num = if player_num > 1 then get_integer (-1) player_num else 0 in
  Pervasives.flush stdout;
  erase_above ();
  ANSITerminal.(print_string [blue] ("\nThe game has "^ 
                                     (string_of_int (player_num - npc_num)) ^ 
                                     " human players and "^ 
                                     (string_of_int (npc_num)) ^ 
                                     " bots 
    For a turn to be valid, all words placed must be in the Scrabble dictionary
and all tiles must be connected to the center (8,8) \n\n") );
  Pervasives.flush stdout;
  Unix.sleepf 2.0;
  ANSITerminal.(print_string [green] help_message);
  Pervasives.flush stdout;
  Unix.sleepf 8.0; 
  erase_above ();

  State.print_board_from_state (State.init_state (player_num) npc_num); 

  print_newline();
  ignore (execute_command (State.init_state player_num npc_num))

(* Execute the game engine. *)
let () = main ()
