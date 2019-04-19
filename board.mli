(** The abstract type for a Scrabble board *)
type t

(** The type representing a letter tile *)
type tile

(** The type representing a position on the board*)
type position 

(** The type representing a location *)
type board_square

(** Raised when the user attempts to place a tile on occupied [board_square] *)
exception Occupied of board_square

(** Raised when [position] is an invalid position in the board *)
exception InvalidPos of position

(** Raised when one of the words placed on the board is not in the dictionary*)
exception BadWord

(** Raised when some of the tiles are not connected to the center tile*)
exception NotConnected

(** Raised when only one letter placed in the center of the board and the user
  * attempts to end their turn. *)
exception OneLetter

(** Raised when the user tries to place a tile which is not an alphabetically
  * character *)
exception InvalidChar

(** [get_x square] returns the x coordinate of the position of [square]*)
val get_x : board_square -> int

(** [get_y square] returns the y coordinate of [square]*)
val get_y : board_square -> int

(** [get_pos square] returns the position coordinate of [square]*)
val get_pos : board_square -> position

(** [get_occ] returns the tile option at [square]*)
val get_occ : board_square -> tile option

(** [set_occ] returns the tile option at [square]*)
val set_occ : board_square -> tile -> board_square

(** Takes in a board [t] and prints it to console *)
val print_board : t -> unit

(** [set_square t p b] adds tile [t] to position [p] of board [b] if that 
    position is not occupied, and returns a new board. If [p] is occupied, 
    raises [Occupied] *)
val set_square : tile -> position -> t -> t

(** [get_square pos board] returns the board_square at position [pos] in [t].
  * If there exists no position [pos] in [t], raises   *)
val get_square : position -> t -> board_square

(** [is_valid_board b] returns true if all the following conditions are true, 
    false otherwise:
    1. All words in board [b] are words in the Scrabble dictionary. 
    2. All occupied board squares in [b] are adjacent to at least one other 
    occupied board square 
    3. There is a path from the center board square to any other occupied 
    board square *)
val is_valid_board : t -> bool

(** [new_board len] Return an empty board of size [len]x[len]*)
val new_board : int -> t

(** [make_board_square c x y] returns a [board_square] at position [(x,y)] 
  * occupied by [c] *)
val make_board_square : char option -> int -> int -> board_square

(** [merg_boards board1 board2] adds the occupied squares in [board1] to 
    [board2] if they are unoccupied. Requires that [board1] be a subset of 
    [board2] *)
val merge_boards : board_square list -> t -> t

(** [make_pos col row] returns the position at ([row], [col])*)
val make_pos : int -> int -> position

(** [make_tile c] returns the tile with character c*)
val make_tile : char -> tile

(** [get_board_score board] returns the score value of all the words on [board]*)
val get_board_score : t -> t -> int

(** prints the character on a tile*)
val print_tile : tile -> unit

(** [get_board_word_diff old_board new_board] returns the list of words that is
    different between [old_board] and [new_board]*)
val get_board_word_diff : t -> t -> string list