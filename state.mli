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

(** The type representing which arrow key the user pressed when attempting
  * to change the location of the cursor. *)
type cursor_change = 
  | Left
  | Right
  | Up
  | Down
  | Invalid

(** Raised when the user does not press any of the arrow keys (i.e. position
  * of the cursor does not change) *)
exception NoCursorChange

(** The exception type returned when a tile is placed in a way that is illegal*)
exception MisplacedTile

(** Raised when the tile bag is empty, which we are treating as the end 
  * condition in this sprint of the assignment (for simplicity).  *)
exception EndGame

(** Raised when the player tries to place a tile on a preoccupied board square*)
exception Occupied

(** Raised when the player tries to place a tile which they don't have in their
  * inventory *)
exception NotInInv

(** Raised in the beginning of the game if the user tries to initialize a game
  * with less than two players. *)
exception InvalidNumPlayers

(** [init_state num_players] is the initial state of the Scrabble game with
  * [num_players] players. *)
val init_state : int -> t 

(** [place_tile t p curr_turn] updates the curr_turn with tile t at 
    position p and removes it from the current player's inventory. 
    Raises Misplaced_tile is the player places a tile illegally (i.e. if the
    tile is placed so that it is disconnected from previously placed tiles) *)
val place_tile : Board.tile -> Board.position -> t -> t

(** [remove_tile p st] removes the tile at position [p] from the placed 
    tiles in the current turn in state [t].  *)
val remove_tile : Board.position -> t -> t

(** [end_turn] returns a new state based on the actions performed by the 
    player in curr_turn. If the conditions for a valid board are met,
    the current player's score is updated and their inventory is replenished. 
    Otherwise, prints a helpful error message and the players turn continues. 

    Raises: *)
val end_turn : t -> t

(** [get_cursor_xpos st] returns the x position of the cursor in state [st] *)
val get_cursor_xpos : t -> int

(** [get_cursor_ypos st] returns the y position of the cursor in state [st] *)
val get_cursor_ypos : t -> int

(** [get_scores] returns a tuple containing the score for each player 
    in the game in the form 
    [[(player1, player1's score); (player2, player2's score); ...]] . *)
val get_scores : t -> (int*int) list

(** [get_score st player_id] returns the score for player with player id 
  * [player_id] in [st]. *)
val get_score : t -> int -> int

(** [print_inventory state] prints the current player's inventory *)
val print_inventory : t -> unit

(** [print_board_from_state st] prints the Scrabble board in state [st] *)
val print_board_from_state : t -> unit

(** [get_curr_player_id st] returns the current player's id in state [st] *)
val get_curr_player_id : t -> int

(** [print_scores state] prints the scores for each player*)
val print_scores : t -> unit

(** [print_winner state] prints the winner, who is the player with the most
  * number of points when the game ends. *)
val print_winner: t -> unit

(** [get_state_word_diff old_state new_state] returns the new words formed
     on the board in [new_state] by returning a list of strings which are
     words on the board in [new_state] are not words on the board in
    [old_state] *)
val get_state_word_diff : t -> t -> string list

(** [get_state_score_diff old_state new_state] returns the new score gained
    from the newly formed words in [new_state] by substracting the sum of the
    scores in [new_state] from the sum of the scores in [old_state] *)
val get_state_score_diff : t -> t -> int

(** [new_state_with_cursor_change st change] returns a new state with updated
  * cursor position based on the cursor change indicated by [change]. *)
val new_state_with_cursor_change : t -> cursor_change -> t 

val perfect_turn : t -> t


