open Board 

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

exception NotInInv of tile
exception MisplacedTile
exception Occupied

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

(** [is_in_line tile curr_turn] returns true if tile is in the same column or
    in the same row as all of the other newly placed tiles in the turn*)
let is_in_line (square:board_square) (curr_turn:curr_turn) = 
  if (same_x square curr_turn || same_y square curr_turn) = true then true else
    raise MisplacedTile 

(** [remove_from_inv tile curr_turn] returns the current player's inventory
    without [tile]. Riases NotInInv if tile is not in the player's inventory*)
let remove_from_inv (tile:tile) (curr_turn:curr_turn) = 
  let curr_player = curr_turn.curr_player in
  let inv = curr_player.inv in 
  let rec loop acc tile inv =
    match inv with
    |h::t when h = tile -> acc @ t
    |h::t -> loop (h::acc) tile t
    |[] -> raise (NotInInv tile)
  in loop [] tile inv  

let place_tile (tile:tile) (pos:position) (state:t) = 
  if is_in_inv tile state.curr_turn then
    let board_square = get_square pos state.board in 
    if get_occ board_square = None then
      if is_in_line board_square state.curr_turn then 
        let new_inv = remove_from_inv tile state.curr_turn in
        let new_square = set_occ board_square tile in
        let new_curr_turn = 
          {curr_player = {state.curr_turn.curr_player with inv= new_inv}; 
           new_squares = new_square::state.curr_turn.new_squares} in 
        {state with curr_turn=new_curr_turn}
      else state
    else state
  else state


let remove_tile (pos:position) (curr_turn:curr_turn) = 
  failwith "unimplemented"


(** [from_bag_to_inv bag inv] returns a tuple whose first element is a tile list
  * representing the new tile bag and whose second element is a tile list 
  * representing the new inventory of the current player. *)
let rec from_bag_to_inv (bag: tile list) (inv: tile list) = 
  let size = List.length inv in
  if (size=7) then (bag,inv) else
    let bag_size = List.length bag in   
    let rand_tile = List.nth tile_bag (Random.int bag_size) in 
    let new_tile_bag = List.filter (fun tile -> tile<>rand_tile ) bag in 
    let new_inv = rand_tile::inv in 
    from_bag_to_inv new_tile_bag new_inv


(** [replenish_inventory board] returns a new board with the current player's
  * inventory of tiles updated, where the additional tiles added to that player's
  * inventory were randomly selected and removed from the tile bag *)
let rec replenish_inventory (board: t) : t = 
  let tile_bag = board.tile_bag in
  let player = board.curr_turn.curr_player in 
  let current_inventory = player.inv in
  let new_data = from_bag_to_inv tile_bag current_inventory in 
  let new_bag = fst new_data in 
  let new_inv = snd new_data in
  let updated_player: player = {player_id = player.player_id; score = player.score;
                                inv = new_inv;} in 
  let players = List.filter (fun player -> player.player_id<>updated_player.player_id) board.players in 
  let players = updated_player::player in 
  { 
    players = players; 
    board = board.board;
    curr_turn = board.curr_turn;
    tile_bag = new_bag;
  }

let end_turn (curr_turn:curr_turn) (curr_turn:curr_turn) = 
  failwith "Unimplemented"






