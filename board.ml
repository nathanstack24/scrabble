
type tile = char

type position = int*int

type board_square = {
  pos : position; 
  occ : tile option
}

type t = board_square list

exception Occupied of board_square
exception InvalidPos of position

let get_x (square:board_square) = 
  fst square.pos

let get_y (square:board_square) = 
  snd square.pos

let get_pos (square:board_square) = 
  square.pos

let get_occ (square:board_square) = 
  square.occ

let set_occ (square:board_square) (tile:tile) = 
  {pos=get_pos square; occ= Some tile}

let print_board (board:t) = 
  failwith "Unimplemented"

let rec get_square (pos:position) (board:t) = 
  match board with 
  | [] -> raise (InvalidPos pos)
  | h::t -> if (h.pos = pos) then h else get_square pos t

let rec set_square_helper  (acc:t) (tile:tile) (pos:position) (board:t)  = 
  match board with 
  | [] -> raise (InvalidPos pos)
  | h::t -> if (h.pos=pos) then 
      let new_square: board_square = {pos=pos; occ= Some tile} in 
      (new_square::acc) @ t else 
      set_square_helper (h::acc) tile pos t

let make_board_square (c:char) (p:int*char) : board_square = 


  let set_square (tile:tile) (pos:position) (board:t) = 
    set_square_helper []

let is_valid_board (board:t) = 
  failwith "Unimplemented"

let new_board n = []

(* let rec new_board_helper acc n = ()*)