
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
  failwith "Unimplemented"

let set_square (tile:tile) (pos:position) (board:t) = 
  set_square_helper []

(** [connected_to_center center_pos board] Returns a list of all of the occupied
    board_squares with a path from the center center*)
let connected_to_center (center_pos:position) (board:t)=
  let rec bfs (connected:position list) (visited:position list) (queue:board_square list) (pos_list:position list)= 
    match queue with 
    |{pos = p; occ = Some tile}::t ->
      (* Check that there is > 0 elements within 1 square of the square
         being visited*)
      let neigh_list = (List.filter (fun p_neigh -> 
          (snd p = snd p_neigh && 
           (fst p = fst p_neigh + 1 || fst p = fst p_neigh - 1)) ||
          (fst p = fst p_neigh && 
           (snd p = snd p_neigh + 1 || snd p = snd p_neigh - 1 )) &&
          List.mem p_neigh visited = false) pos_list) in 
      bfs (neigh_list @ connected) (p::visited) t pos_list
    |{pos = p; occ = _ }::t -> bfs connected (p::visited) t pos_list
    |[] -> connected
  in bfs [] [] ((get_square center_pos board)::[]) (List.map (fun square -> get_pos square) board)


let is_valid_board (board:t) = 
  failwith "Unimplemented"

let new_board n = []

(* let rec new_board_helper acc n = ()*)