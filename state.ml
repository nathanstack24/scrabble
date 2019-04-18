open Board 
open Dictionary

type player = {
  player_id: int;
  score: int; 
  inv: tile list}

type curr_turn = {
  curr_player: player;
  new_squares: board_square list;
}

type t = {
  players : player list; 
  board : Board.t;
  curr_turn: curr_turn;
  tile_bag: tile list;
}

exception MisplacedTile
exception EmptyBoardSquare
exception EndGame
exception Occupied
exception NotInInv

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


let init_state (num_players: int) : t =
  if num_players<2 then failwith "Error: need at least two players" else 
    let rec create_players (num:int) (acc:player list) (bag: tile list) = 
      if num=0 then (bag,acc) else 
        let new_data = from_bag_to_inv bag [] in 
        let new_bag = fst new_data in 
        let new_inv = snd new_data in 
        let new_player: player = {player_id=num; score=0; inv=new_inv} in 
        create_players (num-1) (new_player::acc) new_bag  in 
    let data = create_players num_players [] (make_tile_bag init_tile_bag []) in 
    let bag = fst data in 
    let players = snd data in
    let new_curr_turn = {curr_player=(List.hd players); new_squares=[]} in 
    {
      players = players;
      board = new_board 15;
      curr_turn = new_curr_turn;
      tile_bag = bag;
    }

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
        {state with curr_turn=new_curr_turn}
      else state
    else raise Occupied
  else raise NotInInv

let remove_tile (pos:position) (board:t) = 
  let placed_squares = board.curr_turn.new_squares in 
  let player = board.curr_turn.curr_player in 
  (** TODO: implement error handling with this case. Raises Not_Found if no
      square placed by the user in this turn is located at pos *)
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

(** [comp_players player1 player2] compares player1 and player2 based on their
    player_id*)
let comp_players player1 player2 =
  if player1.player_id = player2.player_id then 0
  else if player1.player_id > player2.player_id then 1 else -1

(** [get_new_score old_boad new_board] returns the additional points a player
    earned based on the board before their turn and after their turn*)
let get_new_score old_board new_board = 
  Board.get_board_score old_board new_board

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
  let new_score = get_new_score curr_board new_board in
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

(** [next_player curr_id players] returns the next player. *)
let rec next_player curr_id players = 
  let next_id = curr_id + 1 in 
  try get_player_from_id next_id players with
  |Not_found -> get_player_from_id 1 players

(** prints a list*)
let rec print_tile_list = function 
    [] -> ()
  | e::l -> print_tile e ; print_string " " ; print_tile_list l

let end_turn state =
  (* TODO: add more explicit error handling for this case in is_valid_board *)
  if (List.length state.curr_turn.new_squares <= 1) then raise Board.BadWord else 
    let curr_board = state.curr_turn.new_squares in 
    let merged_board = merge_boards curr_board state.board in 
    let curr_player = state.curr_turn.curr_player in 
    let id = curr_player.player_id in 
    let next = next_player id state.players in 
    if is_valid_board merged_board = true then 
      let new_data = replenish_inventory state in
      let new_players = fst new_data in 
      let new_bag = snd new_data in 
      {players = new_players; 
       board = merged_board;
       curr_turn = {curr_player = next; new_squares = []};
       tile_bag = new_bag} 
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

let print_scores (state:t) = 
  let rec loop players = 
    match players with 
    |[] -> print_newline ();
    |h::t -> print_endline ("Player " ^ (string_of_int h.player_id) ^ ": " ^  
                            (string_of_int h.score)); (loop t)
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

(** [add_curr_turn st bsq] returns a new state after adding the board square 
  * [bsq] to state [st]'s curtur's newsquares.*)
let add_curr_turn st bsq = 
  let newsq = (List.cons bsq st.curr_turn.new_squares) in 
  let ct = st.curr_turn in 
  {st with curr_turn={ct with new_squares=newsq}}

let print_board_from_state st = Board.print_board 
    (merge_boards st.curr_turn.new_squares st.board)


let print_inventory st = print_tile_list st.curr_turn.curr_player.inv


let get_curr_player_id st = st.curr_turn.curr_player.player_id

