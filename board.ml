
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
  set_square_helper [] tile pos board

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
  in bfs [] [] ((get_square center_pos board)::[]) 
    (List.map (fun square -> get_pos square) board)

(** [first_letter_squares_pos board] returns the positions of the first letters
    of each word on the board*)
let first_letter_squares_pos (board:t) = 
  let rec loop (unvisited:board_square list) (acc:position list) board= 
    match unvisited with 
    |[] -> []
    |{pos=p; occ = None}::t -> loop t acc board
    |h::t when (get_x h = 1 || get_y h = 1)-> loop t ((get_pos h)::acc) board
    |{pos=p; occ = Some tile}::t ->  
      let up:position = (fst p, snd p + 1) in 
      let left:position = (fst p + 1, snd p) in 
      if get_occ (get_square up board) = None &&
         get_occ(get_square left board) = None then
        loop t (p::acc) board else loop t acc board
  in loop board [] board 

(** [get_board_row r board] returns all the board_squares in [board] in row 
    [r]*)
let get_board_row r board = 
  let rec loop r square_list acc =
    match square_list with 
    |[] -> acc
    |h::t -> if get_y h = r then (loop r t (h::acc)) else loop r t acc
  in loop r board []

(** [get_board_col c board] returns all the board_squares in [board] in column 
    [c]*)
let get_board_col c board = 
  let rec loop c square_list acc = 
    match square_list with
    |[] -> acc
    |h::t -> if get_x h = c then (loop c t (h::acc)) else loop c t acc
  in loop c board []

let rec find_word_row (first_letter_pos:position) (board:t)= 
  let row_ind = snd first_letter_pos in
  let row = get_board_row row_ind board in 
  let col_ind = fst first_letter_pos in
  let rec loop lst c r word = 
    match lst with 
    |{pos = (c, r)}



let is_valid_board (board:t) = 
  failwith "Unimplemented"


(** [new_board_helper r c n] defines a new list of empty board squares with 
    [r] rows and [c] columns. [n] = [c] allows the original value of [c] to 
    persist and continue be used throughout the recursion*)
let rec new_board_helper r c n: t =
  if r = 0 then []
  else if c = 0 then new_board_helper (r-1) n n
  else let new_square = {pos = (c,r); occ = None} in 
    new_square::new_board_helper r (c-1) n 

let new_board n : t = 
  new_board_helper n n n

(* let rec new_board_helper acc n = ()*)