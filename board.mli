
type t

(** The type representing a letter tile *)
type tile

(** The type representing a position on the board*)
type position 

(** The type representing a location *)
type board_square

(** Raised when the user attempts to place a tile on an occupied board square *)
exception Occupied of board_square

(** Takes in a board [t] and prints it to console *)
val print_board : t -> unit

(** [set_square t p b] adds tile [t] to position [p] of board [b] if that 
    position is not occupied. If [p] is occupied, raises [Occupied] *)
val set_square : tile -> position -> t -> t

(** Gets the boardsquare at [position] in [t] *)
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

val new_board : unit -> t
