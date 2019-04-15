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




(** [i -- j] is the list of integers from [i] to [j], inclusive.
    Tail recursive. *)
let (--) (i : int) (j : int) : int list =
  let rec from i j l =
    if i>j then l
    else from i (j-1) (j::l)
  in from i j []

(** [shuffle lst] is a random permutation of [lst]. *)
let shuffle (lst : 'a list) : 'a list =
  QCheck.Gen.(generate1 (shuffle_l lst))

(** [replenish_inventory board] returns a new board with the current player's
  * inventory of tiles updated, where the additional tiles added to that player's
  * inventory were randomly selected and removed from the tile bag *)
let replenish_inventory (board: t) = 
  let tile_bag = board.tile_bag in
  let current_inventory = board.curr_turn.curr_player.inv in 
  let size = List.length current_inventory in 
  let int_list = 1 -- size in 
  let shuffled = shuffle int_list in 
  match shuffled with 
  | [] ->
  | h::t -> 

    let end_turn (curr_turn:curr_turn) (curr_turn:curr_turn) = 
      failwith "unimplemented"





