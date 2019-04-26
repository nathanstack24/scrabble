open Board 
open Dictionary

type player = {
  player_id: int;
  score: int; 
  inv: tile list;
  bot: bool}

type curr_turn = {
  curr_player: player;
  new_squares: board_square list;
}

type t = {
  players : player list; 
  board : Board.t;
  curr_turn: curr_turn;
  tile_bag: tile list;
  cursor: position
}

type cursor_change = 
  | Left
  | Right
  | Up
  | Down
  | Invalid


exception MisplacedTile
exception EmptyBoardSquare
exception EndGame
exception Occupied
exception NotInInv
exception InvalidNumPlayers
exception InvalidPlayerID
exception NoCursorChange
exception CannotSkip
exception Gap

let get_cursor_xpos (st:t) = Board.get_x_pos st.cursor

let get_cursor_ypos (st:t) = Board.get_y_pos st.cursor

(** [is_in_inv t curr_turn] returns whether tile is in the current player's 
    inventory*)
let is_in_inv (tile:tile) (curr_turn:curr_turn) = 
  let curr_player = curr_turn.curr_player in
  let inv = curr_player.inv in 
  if List.mem tile inv then true else false

(** [same_x tile curr_turn] returns true if tile is in the same column 
    as all of the other newly placed tiles in the turn*)
let same_x (square: board_square) (curr_turn:curr_turn) = 
  let new_squares = curr_turn.new_squares in
  let sq_x = get_x square in 
  let rec loop acc sq_x new_squares = 
    match new_squares with 
    |[] -> acc
    |h::t -> let x = get_x h in 
      loop (acc && (x=sq_x)) sq_x t
  in loop true sq_x new_squares

(** [same_y tile curr_turn] returns true if tile is in the same row 
    as all of the other newly placed tiles in the turn*)
let same_y (square: board_square) (curr_turn:curr_turn) = 
  let new_squares = curr_turn.new_squares in
  let sq_y = get_y square in 
  let rec loop acc sq_y new_squares = 
    match new_squares with 
    |[] -> acc
    |h::t -> let y = get_y h in 
      loop (acc && (y=sq_y)) sq_y t
  in loop true sq_y new_squares

(** [get_occ_state sq] returns the tile option at [sq]. Checks both the board
  * itself and the tiles placed by the player. *)
let get_occ_state (state: t) (square:board_square) : tile option = 
  let placed_tiles = state.curr_turn.new_squares in 
  match get_occ square with 
  | None -> (try 
               let tile = List.find 
                   (fun tile -> (get_pos tile)=(get_pos square)) placed_tiles in 
               get_occ tile
             with Not_found -> None)
  | Some tile -> Some tile

(** [is_in_line tile curr_turn] returns true if tile is in the same column or
    in the same row as all of the other newly placed tiles in the turn 
    Otherwise raises MisplacedTile*)
let is_in_line (square:board_square) (curr_turn:curr_turn) = 
  if (same_x square curr_turn || same_y square curr_turn) = true then true else
    raise MisplacedTile 

(** [comp_players player1 player2] compares player1 and player2 based on their 
    id*)
let comp_players player1 player2 =
  if player1.player_id = player2.player_id then 0
  else if player1.player_id > player2.player_id then 1 else -1

(** [get_new_score old_boad new_board] returns the additional points a player
    earned based on the board before their turn and after their turn*)
let get_new_score old_board new_board = 
  Board.get_board_score old_board new_board

(** [remove_from_tile_list tile lst] returns the tile list [lst] without the 
    first occurence of [tile]. Raises Not_found if tile is not in the tile 
    list *)
let remove_from_tile_list (tile:tile) (lst: tile list) = 
  let rec loop acc tile lst =
    match lst with
    |h::t when h = tile -> acc @ t
    |h::t -> loop (h::acc) tile t
    |[] -> raise (Not_found)
  in loop [] tile lst  

(** [make_tile_bag char_lst acc] returns the list of tiles corresponding to a
    list of chars*)
let rec make_tile_bag (char_lst: char list) (acc:tile list) : tile list = 
  match char_lst with
  |[] -> acc
  |h::t -> make_tile_bag t ((make_tile h)::acc)

let new_state_with_cursor_change (st: t) (change:cursor_change) = 
  let pos = st.cursor in 
  let x = get_x_pos pos in 
  let y = get_y_pos pos in
  match change with 
  | Left -> if x=1 then st else {st with cursor=(make_pos (x-1) y)}
  | Right -> if x=15 then st else {st with cursor=(make_pos (x+1) y)}
  | Up -> if y=15 then st else {st with cursor=(make_pos x (y+1))}
  | Down -> if y=1 then st else {st with cursor=(make_pos x (y-1))}
  | Invalid -> failwith 
                 "Error: tried to initialize new state with invalid cursor"

(** [from_bag_to_inv bag inv] returns a tuple whose first element is a tile list
  * representing the new tile bag and whose second element is a tile list 
  * representing the new inventory of the current player. Tiles from the tile
  * bag are randomly chosen and added to the user's inventory until the user has
  * 7 tiles in their inventory (as the rules of scrabble require) *)
let rec from_bag_to_inv (bag: tile list) (inv: tile list) = 
  let size = List.length inv in
  if (size=7) then (bag,inv) else
    let bag_size = List.length bag in
    if bag_size=0 then raise EndGame else 
      Random.init (int_of_float (Unix.time ()));
    let rand_tile = List.nth bag (Random.int bag_size) in 
    let new_tile_bag = remove_from_tile_list rand_tile bag in 
    let new_inv = rand_tile::inv in 
    from_bag_to_inv new_tile_bag new_inv

let init_state (num_players: int) (num_bots:int): t =
  if (num_players<2 || num_players>4) then raise InvalidNumPlayers else 
    let rec create_players (num:int) (num_bot:int) (acc:player list) 
        (bag: tile list) = 
      if num=0 then (bag,acc) else 
        let new_data = from_bag_to_inv bag [] in 
        let new_bag = fst new_data in 
        let new_inv = snd new_data in 
        let new_player: player = 
          (if num_bot <= 0 then 
             {player_id=num; score=0; inv=new_inv; bot = false} else 
             {player_id=num; score=0; inv=new_inv; bot = true}) in
        create_players (num-1) (num_bot-1)(new_player::acc) new_bag  in 
    let data = create_players num_players num_bots [] 
        (make_tile_bag init_tile_bag []) in 
    let bag = fst data in 
    let players = snd data in
    let new_curr_turn = {curr_player=(List.hd players); new_squares=[]} in 
    {
      players = players;
      board = new_board 15;
      curr_turn = new_curr_turn;
      tile_bag = bag;
      cursor = make_pos 8 8;
    }
let place_tile (tile:tile) (pos:position) (state:t) = 
  if is_in_inv tile state.curr_turn then
    let board_square = get_square pos state.board in 
    if get_occ_state state board_square = None then
      if is_in_line board_square state.curr_turn then 
        let new_inv = remove_from_tile_list 
            tile state.curr_turn.curr_player.inv in
        let new_square = set_occ board_square tile in
        let new_curr_turn = 
          {curr_player = {state.curr_turn.curr_player with inv= new_inv}; 
           new_squares = new_square::state.curr_turn.new_squares} in 
        {state with curr_turn=new_curr_turn; cursor=pos}
      else state
    else raise Occupied
  else raise NotInInv

let remove_tile (pos:position) (board:t) = 
  let placed_squares = board.curr_turn.new_squares in 
  let player = board.curr_turn.curr_player in 
  let tile = List.find (fun square -> (get_pos square) = pos) placed_squares in
  let new_placed_squares = List.filter 
      (fun square -> (get_pos square)<>pos) placed_squares in 
  match (get_occ tile) with 
  | None -> raise Not_found
  | Some t -> let tile = t in 
    let new_player = {player with inv=tile::board.curr_turn.curr_player.inv } in
    let new_curr_turn = {
      curr_player= new_player;
      new_squares=new_placed_squares;} in 
    { board with curr_turn = new_curr_turn }

(** [replenish_inventory board] returns a new player list with the current 
 * player's inventory of tiles updated, where the additional tiles added to 
 * that player's inventory were randomly selected and removed from the tile 
 * bag *)
let rec replenish_inventory (board: t) : player list * tile list = 
  let player = board.curr_turn.curr_player in 
  let new_data = from_bag_to_inv board.tile_bag player.inv in 
  let new_bag = fst new_data in 
  let new_inv = snd new_data in
  let curr_board = board.board in 
  let new_board = merge_boards board.curr_turn.new_squares curr_board in 
  let new_score = Board.get_board_score curr_board new_board in
  let updated_player = {player with inv = new_inv; 
                                    score=(player.score + new_score); } in 
  let players = List.filter 
      (fun p -> p.player_id<>updated_player.player_id) board.players in 
  (List.sort comp_players (updated_player::players), new_bag)

(** [get_player_from_id id player_list] returns the player in [player_list] 
  * with player id [id] *)
let rec get_player_from_id id player_list= 
  match player_list with
  |h::t when h.player_id = id -> h
  |h::t -> get_player_from_id id t
  |[] -> raise Not_found

(** [next_player curr_id players] returns the next player (the player whose
  * turn it is after the current player ends their turn). *)
let rec next_player curr_id players = 
  let next_id = curr_id + 1 in 
  try get_player_from_id next_id players with
  |Not_found -> get_player_from_id 1 players

let skip_curr_turn state = 
  let curr_player = state.curr_turn.curr_player in
  if state.curr_turn.new_squares<>[] then raise CannotSkip else
    let id = curr_player.player_id in 
    let next = next_player id state.players in
    let new_curr_turn = {state.curr_turn with curr_player=next} in 
    {state with curr_turn=new_curr_turn}

let end_turn state =
  let new_tiles = state.curr_turn.new_squares in 
  let merged_board = merge_boards new_tiles state.board in 
  let curr_player = state.curr_turn.curr_player in 
  let id = curr_player.player_id in 
  let next = next_player id state.players in 
  if is_valid_board merged_board = true then
    let new_data = replenish_inventory state in
    let new_players = fst new_data in 
    let new_bag = snd new_data in 
    let new_st = 
      {players = new_players; 
       board = merged_board;
       curr_turn = {curr_player = next; new_squares = []};
       tile_bag = new_bag;
       cursor = make_pos 8 8;}  in 
    Board.remove_tiles_from_premiums new_tiles;
    new_st
  else 
    {state with 
     curr_turn = {curr_player = get_player_from_id id state.players;
                  new_squares = []}}

let get_scores (state:t) = 
  let rec loop players = 
    match players with 
    |[] -> []
    |h::t -> (h.player_id, h.score)::(loop t)
  in loop state.players

let get_score_for_player (state:t) (player_id:int) = 
  let player = List.find (fun player -> player.player_id=player_id) 
      state.players in 
  player.score

let get_curr_player_id st = st.curr_turn.curr_player.player_id

let get_state_word_diff old_state new_state = 
  get_board_word_diff old_state.board new_state.board

let get_state_score_diff (old_state:t) (new_state:t) (player:int) = 
  let new_score = get_score_for_player new_state player in
  let old_score = get_score_for_player old_state player in 
  new_score - old_score

let print_scores (state:t) = 
  let rec loop players = 
    match players with 
    |[] -> print_newline ();
    |h::t -> ANSITerminal.print_string [ANSITerminal.green] 
               ("Player " ^ (string_of_int h.player_id) ^ ": " ^  
                (string_of_int h.score)^ "\n"); (loop t)
  in loop state.players

let print_winner (state:t) = 
  let rec loop players winner : player = 
    match players with 
    | [] -> winner;
    | h::t -> if (h.score>winner.score) then loop t h else loop t winner 
  in 
  let winner = loop state.players (List.hd state.players) in 
  print_endline 
    ("Player " ^(string_of_int winner.player_id) ^ " is the winner!")

let print_board_from_state st = Board.print_board 
    (merge_boards st.curr_turn.new_squares st.board) st.cursor

(** [print_tile_list tile_lst] prints a list of tiles as their characters*)
let rec print_tile_list = function 
    [] -> ()
  | e::l -> print_tile e ; print_string " " ; print_tile_list l

let print_inventory st = print_tile_list st.curr_turn.curr_player.inv

let get_curr_player_id st = st.curr_turn.curr_player.player_id

let get_state_word_diff old_state new_state = 
  get_board_word_diff old_state.board new_state.board

let is_player_bot player_id state= 
  let player = get_player_from_id player_id state.players in 
  player.bot

let get_state_score_diff (old_state:t) (new_state:t) (player:int) = 
  let new_score = get_score_for_player new_state player in
  let old_score = get_score_for_player old_state player in 
  new_score - old_score

(**[place_word_right word pos state] returns a state with [word] on the board 
   with the first letter at [pos] from right to left*)
let rec place_word_right word pos state : t option= 
  match word with 
  |[] -> Some state
  |h::t -> try (let new_state = place_tile h pos state in
                place_word_right t (get_right_pos pos) new_state) with
  |Occupied -> if get_occ (get_square (pos) state.board) = Some h then 
      place_word_right t (get_right_pos pos) state else
      None
  |_ -> None

(**[place_word_down word pos state] returns a state with [word] on the board 
   with the first letter at [pos] from the top*)
let rec place_word_down word pos state : t option= 
  match word with 
  |[] -> Some state
  |h::t -> try (let new_state = place_tile h pos state in
                place_word_down t (get_down_pos pos) new_state) with
  |Occupied -> if get_occ (get_square (pos) state.board) = Some h then 
      place_word_down t (get_down_pos pos) state else
      None
  |_ -> None

(**[find_placements_helper word state acc pos_list] returns a list of all of the
   states where [word] could be placedon the board with [word] placed with its 
   first character in all of the positions in [pos_list]*)
let rec find_placements_helper word (state:t) (acc:t list) = function
  | [] -> acc
  | h::t -> match ((place_word_right word h state), 
                   (place_word_down word h state)) with 
  | (Some s1, Some s2) -> find_placements_helper word state (s1::s2::acc) t
  | (Some s1, None) -> find_placements_helper word state (s1::acc) t
  | (None, Some s2) -> find_placements_helper word state (s2::acc) t
  | (None, None) -> find_placements_helper word state acc t

(**[find_placements word state] returns a list with all the states where [word]
   is placed on the board*)
let rec find_placements word (state:t) = 
  let clist = List.rev (make_tile_bag (List.init (String.length word) 
                                         (String.get word)) []) in 
  find_placements_helper clist state [] (get_board_positions state.board)

(** [best_state state_lst init_state] returns the state in [state_lst] that will
    give the highest score compared to [init_state] *)
let best_state state_lst init_state = 
  let rec loop max_score_state state_lst init_state max_score=
    match state_lst with 
    |[] -> max_score_state 
    |state::t -> 
      let merged_board = merge_boards state.curr_turn.new_squares state.board in 
      let curr_score = get_new_score init_state.board merged_board in 
      if curr_score >  max_score 
      then  loop state t init_state curr_score 
      else loop max_score_state t init_state max_score
  in loop init_state state_lst init_state 0

let perfect_turn init_state = 
  let possible_words = Board.possible_words_dict 
      (init_state.curr_turn.curr_player.inv) init_state.board in
  let rec loop word_list acc = 
    match word_list with
    |h::t -> let placements = find_placements h init_state 
      in loop t (placements @ acc )
    |[] -> acc 
  in
  let possible_states = loop possible_words [] in 
  let connected_states = List.filter (fun s -> 
      let merged = merge_boards s.curr_turn.new_squares s.board in 
      are_connected_to_center (make_pos 8 8)  merged) possible_states in
  let valid_states = List.filter (fun s -> 
      let merged = merge_boards s.curr_turn.new_squares s.board in 
      check_words merged) connected_states in 
  best_state valid_states init_state

