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

(** Returns true if all the following conditions are true, false otherwise:
    1. All words are actually words. 
    2. All occupied board_squares are adjacent to at least one other 
    occupied board_square 
    3. There is a path from the center board_square to any other occupied 
    board square 

    ALGO IDEAS:
    a. First, make a list of occupied board squares. Then, recursively iterate 
    from the start square through all of the squares in adjacent, add them to 
    the list of board squares. Check the equivalence of these two lists.
    b. Next, to check words iterate through all rows & cols, add a word when 
    there is a group of occupied board squares that are broken by an
    unoccupied board square. 
*)
val is_valid_board : t -> bool

(** Return an empty board full of size [n]x[n]*)
val new_board : int -> t

val make_board_square : char option -> int -> int -> board_square

(** [merg_boards board1 board2] adds the occupied squares in [board1] to 
    [board2] if they are unoccupied. Requires that [board1] be a subset of 
    [board2] *)
val merge_boards : board_square list -> t -> t

(** [make_pos col row] returns the position at ([row], [col])*)
val make_pos : int -> int -> position

(** [make_tile c] returns the tile with character c*)
val make_tile : char -> tile