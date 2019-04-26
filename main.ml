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

(** [erase_above ()] is used to maintain the cleanliness of the UI by 
  * erasing all text on the terminal above the current position of the cursor
  * and resetting the cursor position to the top of the screen.  *)
let erase_above = fun () ->
  ANSITerminal.erase Above;
  ANSITerminal.set_cursor 1 2

(** [default] is the default setting for Unix.terminal_io  *)
let default = Unix.tcgetattr Unix.stdin

(** [set_canonical b] sets canonical mode on if [b]=true. Otherwise, it sets
  * canonical mode to false. By default, the terminals buffers its input, so 
  * that it will only send characters after receiving a newline. Setting 
  * canonical mode to false allows the program to process character input
  * without waiting for a newline. *)
let set_canonical on =
  let attr = Unix.tcgetattr Unix.stdin in
  let attr' = { attr with c_icanon = on } in
  Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH attr'

(** [detect_cursor_changes ()] reads user input and moves the cursor based on
  * which character the user inputs; it returns a [cursor_type] based on which
  * key is pressed. *)
let detect_cursor_changes = fun () ->
  set_canonical false;
  match Pervasives.input_char stdin with
  | 'a' -> Left
  | 'd' -> Right
  | 'w' -> Up
  | 's' -> Down
  | '\n' -> raise NoCursorChange
  | _ -> Invalid

(** [press_any_key ()] waits for the user to press any key and then erases
  * all text above the current cursor position. *)
let rec press_any_key = fun () ->
  set_canonical false;
  match Pervasives.input_char stdin with
  | _ -> erase_above(); set_canonical true

(** [help_message] is the string printed to the user when the 'help' command
  * is entered. *)
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
    endturn (ends the current player's turn if it is valid)
    skip (skips the current player's turn)
    help (prints this message to the console)
    quit (quits the game) \n \n"

(** [get_next_command player st] gets the next command from the user, where the
  * current player is [player] and the state of the game is [st]. *)
let rec get_next_command (player_id:int) (st:State.t) = 
  print_endline ("Your turn, Player " ^ string_of_int (player_id)) ;
  let score = List.assoc player_id (State.get_scores st) in
  print_endline ("Your score is: " ^ (string_of_int score));
  print_string ("Your letters are: " ); State.print_inventory st;
  print_newline();
  print_string "> "; flush stdout;
  try 
    (let change = detect_cursor_changes () in
     set_canonical true;
     if change=Invalid then (erase_above ();
                             State.print_board_from_state st; 
                             print_newline(); get_next_command player_id st) 
     else
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
       | Board.InvalidChar -> ANSITerminal.print_string [ANSITerminal.red]
                                ("Bad command. You can only place " ^ 
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
         ANSITerminal.print_string [ANSITerminal.red] 
           "Bad command. Enter 'help' for a list of valid commands";
         flush stdout;
         Unix.sleep 1;
         erase_above ();
         State.print_board_from_state st; 
         print_newline(); get_next_command player_id st)


(** [execute_command] executes the command issued by the user in state [st], 
  * returning the new state updated by the effects of the command. *)
let rec execute_command (st:State.t) : State.t =
  let player_id = get_curr_player_id st in 
  if is_player_bot player_id st then ( 
    print_endline ("Player " ^ string_of_int (player_id) ^ 
                   " (Bot) is thinking") ;
    let new_st = State.end_turn (State.perfect_turn st) in
    let new_words = State.get_state_word_diff st new_st in
    let points = State.get_state_score_diff st new_st player_id in
    erase_above();
    State.print_board_from_state new_st; 
    print_newline(); 
    let new_score = get_score_for_player new_st player_id in 
    ANSITerminal.(print_string [green] 
                    ("Player " ^ string_of_int (player_id) 
                     ^ " made the following word(s): " ^ 
                     (word_list_to_string new_words) ^ "\n" ^"Earning " ^ 
                     (string_of_int points) ^ " points \n"^
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
         let player = State.get_curr_player_id st in 
         let points = State.get_state_score_diff st new_st player in
         let new_score = get_score_for_player new_st player in 
         erase_above();
         State.print_board_from_state new_st; 
         print_newline(); 
         ANSITerminal.(print_string [green] 
                         ("Great turn! \n" ^
                          "You made the following word(s): " 
                          ^ (word_list_to_string new_words) ^ "\n" ^
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
      Unix.sleep 1;
      ANSITerminal.(print_string 
                      [black] "Press any key to dismiss this message");
      flush stdout;
      press_any_key();
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
                          ^"board. Remove all placed tiles and then try"^ 
                          "again. \n")); 
         flush stdout;
         Unix.sleep 2;
         erase_above ();
         State.print_board_from_state st; 
         print_newline(); execute_command st)

    (*| Command.Swap n -> 
      (try 
         let num = (int_of_string n) in 
         if (num > 0 && num < 8) then 
           (let new_st = swap_num_tiles num;
              ANSITerminal.(print_string 
                              [green] ("Swapped "^ 
                                       (string_of_int num)^" tiles!"));
              flush stdout;
              Unix.sleep 2;
              erase_above ();
              State.print_board_from_state new_st; 
              print_newline(); execute_command new_st)
         else
           raise Failure "Bad Swap Num"
       with Failure _ -> 
         ANSITerminal.(print_string [red] ("Please specify a" ^
                                           " valid number of tiles to exchange (1-7)"));
    *)

    |Command.Perfect -> let new_st = State.perfect_turn st in 
      erase_above ();
      State.print_board_from_state new_st; 
      print_newline(); execute_command new_st)


(** [parse_num_players str] returns the integer entered by the user when
  * specifying the numbers of players / bots in the game. 
  * 
  * Raises:
  *    - Empty if the user types nothing
  *    - Malformed if the user does not enter a number *)
let parse_num_players (str: string) = 
  let cmd = (clean_str (String.split_on_char ' ' (String.trim str))) in
  if List.length cmd = 0 then raise Empty else 
    match cmd with 
    | n::[] -> 
      (try (let i = (int_of_string n) in (i)) with | _ -> raise Malformed)
    | _ -> raise Malformed

(** [get_integer low upper str cursor] prints string [str] and then prompts 
    the user to input an integer which is only returned if it is between [low] 
    and [upper] (non-inclusive).  *)
let rec get_integer low upper str = 
  ANSITerminal.(print_string [blue] str );
  match read_line () with
  | exception End_of_file -> exit 1
  | txt -> try 
      (match parse_num_players txt with
       |num when num < upper && num > low -> num
       |_ -> raise Malformed) 
    with 
    | _ -> 
      ANSITerminal.print_string [ANSITerminal.red] 
        ("You must enter an integer between 1 and 4. Try again \n");
      flush stdout;
      Unix.sleep 1;
      erase_above();
      get_integer low upper str

(** [get_num_bots low upper cursor] prompts the user to input an integer which is only
    returned if it is between [low] and [upper] (non-inclusive). If the user
    types an invalid character, an error message is printed, and the function 
    recurses. Although this function is nearly identical to get_integer, it is
    necessary in order to erase the terminal properly.  *)
let rec get_num_bots low upper cursor = 
  match read_line () with
  | exception End_of_file -> exit 1
  | txt -> try 
      (match parse_num_players txt with
       |num when num < upper && num > low -> num
       |_ -> raise Malformed)
    with 
    | _ -> 
      ANSITerminal.print_string [ANSITerminal.red] 
        ("You must enter an integer between "^(string_of_int (low+1))^
         " and "^(string_of_int (upper-1))^". Try again \n");
      flush stdout;
      Unix.sleep 1;
      ANSITerminal.set_cursor (fst cursor) (snd cursor);
      ANSITerminal.erase Below;
      get_num_bots low upper cursor


let main () =
  ANSITerminal.erase Above;
  ANSITerminal.set_cursor 1 1; 
  ANSITerminal.(print_string [black;Blink]
                  "
     ██╗    ██╗███████╗██╗      ██████╗ ██████╗ ███╗   ███╗███████╗
     ██║    ██║██╔════╝██║     ██╔════╝██╔═══██╗████╗ ████║██╔════╝
     ██║ █╗ ██║█████   ██║     ██║     ██║   ██║██╔████╔██║█████╗  
     ██║███╗██║██╔══╝  ██║     ██║     ██║   ██║██║╚██╔╝██║██╔══╝  
      ███╔███╔╝ ██��███╗ ███████╗���███��█╗╚██████╔╝██║ ══╝ ██║███████╗
      ╚══╝╚══╝ ╚══════╝╚══════╝ ╚══���══╝ ╚═════╝ ╚═╝     ╚═╝╚══════╝ 
     \n");
  flush stdout;
  ANSITerminal.(print_string [red;Blink] "
                         ████████╗ █████╗ 
                          ╚═██╔══╝██╔═══██╗
                            ██║   ██║   ██
                            ██║   ██║   ██║
                            ██║   ╚██████╔╝
                            ╚═╝    ╚═════╝ 
     \n");
  flush stdout;
  ANSITerminal.(print_string [black;Blink] " 
   ███████╗ ██████╗██████╗  █████╗ ██████╗ ██████╗ ██╗     ███████╗
   ██╔════╝██╔════╝██╔══██╗██╔══██╗██╔══██╗██╔══██╗██║     ██╔════╝
   ███████ ██|     ██████╔╝███████║██████╔╝██████╔╝██║     █████╗  
   ╚════██║██║     ██╔══██╗██╔══██║██╔══██╗██╔══██╗██║     ██╔══╝  
   ███████║╚██████╗██║  ██║██║  ██║██████╔╝██████╔╝███████╗███████╗
   ╚══════╝ ╚═════╝╚═╝  ╚═╝╚═╝  ╚═╝╚═════╝ ╚═════╝ ╚══════╝╚══════╝ 

                                                                                                                                                                               \n");
  flush stdout;
  Unix.sleepf 1.5;
  ANSITerminal.print_string [ANSITerminal.black] 
    "\n Press any key to continue ";
  flush stdout;
  press_any_key ();
  erase_above();
  ANSITerminal.set_cursor 1 2;
  let player_num_string = "\n\nEnter total number of players (2-4): " in 
  let player_num = get_integer 1 5 player_num_string in
  let bot_num_string = "\n\nOf these, how many bots? (0-"
                       ^string_of_int (player_num-1)^ "): " in 
  ANSITerminal.print_string [ANSITerminal.blue] bot_num_string;
  flush stdout;
  let cursor = ANSITerminal.pos_cursor() in 
  let npc_num = if player_num > 1 then 
      get_num_bots (-1) player_num cursor else 0 in
  erase_above ();
  ANSITerminal.(print_string [blue] ("\nThe game has "^ 
                                     (string_of_int (player_num - npc_num)) ^ 
                                     " human player(s) and "^ 
                                     (string_of_int (npc_num)) ^ 
                                     " bot(s) 
    For a turn to be valid, all words placed must be in the Scrabble dictionary
    and all tiles must be connected to the center (8,8) \n\n") );

  ANSITerminal.(print_string [blue] help_message);
  ANSITerminal.print_string [ANSITerminal.black] 
    "\n Press any key to continue ";
  flush stdout;
  press_any_key();
  erase_above ();

  State.print_board_from_state (State.init_state (player_num) npc_num); 

  print_newline();
  ignore (execute_command (State.init_state player_num npc_num))

(* Execute the game engine. *)
let () = main ()
