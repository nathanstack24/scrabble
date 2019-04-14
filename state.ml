open Board 

type player = {
  player_id: int;
  score: int; 
  inv: tile list}

type curr_turn = {
  curr_player: player;
  new_tiles: board_square list;
}

type t = {
  players : player list; 
  board : board_square list;
  curr_turn: curr_turn;
  tile_bag: tile list;
}

(** [is_in_inv t curr_turn] returns whether tile is in the current player's 
    inventory*)
let is_in_inv (tile:Board.tile) (curr_turn:curr_turn) = 
  let curr_player = curr_turn.curr_player in
  let inv = curr_player.inv in 
  if List.mem tile inv then true else false

let place_tile (t:Board.tile) (pos:Board.position) (curr_turn:curr_turn) = 
  failwith "unimplemented"


let remove_tile (pos:Board.position) (curr_turn:curr_turn) = 
  failwith "unimplemented"

let end_turn (curr_turn:curr_turn) (curr_turn:curr_turn) = 
  failwith "unimplemented"





