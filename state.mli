(** Representation of a dynamic game state

    This module represents the state of a Scrabble game as it is being played, 
    including each player's tiles and score,  the tiles on the board, and the 
    functions that cause the state to change.
*)

(** The abstract type of values representing the game state. *)
type t

(** The type representing a player and their score*)
type player

(** The type representing the actions of a player during their current turn *)
type curr_turn

(** [place_tile t p curr_turn] updates the curr_turn with tile t at 
    position p and removes it from the current player's inventory*)
val place_tile : Board.tile -> Board.position -> curr_turn -> curr_turn

(** [remove_tile p curr_turn] removes the tile at position p from the placed 
    tiles in curr_turn *)
val remove_tile : Board.position -> curr_turn -> curr_turn

(** [replenish_inv t curr_turn] returns a *)
val replenish_inv : t -> curr_turn -> 

  (** [end_turn] returns a new state t based on the actions performed by the 
      player in curr_turn. If the conditions for a valid board are met,
      updates the current player's score and replenishes their inventory, 
      else returns their tiles to them and resets the turn *)
val end_turn : curr_turn -> t -> t





