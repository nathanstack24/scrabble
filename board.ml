type tile = char

type position = int*int

type board_square = {
  pos : position; 
  occ : tile option
}

type t (* TODO: define type t *)

(** Raised when an unknown room is encountered. *)
exception Occupied of board_square

let print_board (board:t) = 
  failwith "Unimplemented"

let set_square (t:tile) (pos:position) (board:t) = 
  failwith "Unimplemented"

let get_square (pos:position) (board:t) = 
  failwith "Unimplemented"

let is_valid_board (board:t) = 
  failwith "Unimplemented"

let new_board() = 
  failwith "Unimplemented"
