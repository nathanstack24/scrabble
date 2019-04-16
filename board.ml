open Dictionary
type tile = char

type position = int*int

type board_square = {
  pos : position; 
  occ : tile option
}

let dict = create_dictionary

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

let make_board_square (c:char) (row:int) (col:int) : board_square = 
  {pos=(row,col); occ=Some c}

let set_square (tile:tile) (pos:position) (board:t): t = 
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

(** [are_connected_to_center center_pos board] returns whether all tiles on 
    the board have a tile path to the center tile on the board*)
let rec are_connected_to_center (center_pos:position) (board:t)=
  let connected_squares = connected_to_center center_pos board in 
  let rec loop connected square_list =
    match square_list with 
    |{pos = p; occ = None}::t -> true && loop connected t
    |{pos = p; occ = Some tile}::t -> (List.mem p connected) && loop connected t
    |[] -> true
  in loop connected_squares board

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

(** comparison function for sorting squares by the x coordinate of their 
    position*)
let comp_squares_x square1 square2 =
  if get_x square1 = get_x square2 then 0
  else if get_x square1 > get_x square2 then 1 else -1

(** comparison function for sorting squares by the y coordinate of their 
    position*)
let comp_squares_y square1 square2 =
  if get_y square1 = get_y square2 then 0
  else if get_y square1 > get_y square2 then 1 else -1

let find_word_row (first_letter_pos:position) (board:t)= 
  let row_ind = snd first_letter_pos in
  let sorted_row = List.sort comp_squares_x (get_board_row row_ind board) in 
  let col_ind = fst first_letter_pos in
  let rec loop lst c word = 
    match lst with 
    |{pos = (x, _); occ = _}::t when x < c -> loop t c word
    |{pos = (x, _); occ = Some tile}::t -> loop t c word^(String.make 1 tile)
    |{pos = (x, _); occ = None}::t -> word
    |[] -> word 
  in loop sorted_row col_ind ""

let find_word_column (first_letter_pos:position) (board:t)= 
  let col_ind = fst first_letter_pos in
  let sorted_col = List.sort comp_squares_y (get_board_row col_ind board) in 
  let row_ind = snd first_letter_pos in
  let rec loop lst r word = 
    match lst with 
    |{pos = (x, _); occ = _}::t when x < r -> loop t r word
    |{pos = (x, _); occ = Some tile}::t -> loop t r word^(String.make 1 tile)
    |{pos = (x, _); occ = None}::t -> word
    |[] -> word 
  in loop sorted_col row_ind ""

let word_list_from_board (board:t) = 
  let first_letters = first_letter_squares_pos board in
  let rec loop pos_list board = 
    match pos_list with
    |[] -> []
    |h::t ->
      let row_word = find_word_row h board in 
      let col_word = find_word_column h board in 
      let row_word_len = String.length row_word in
      let col_word_len = String.length col_word in 
      if row_word_len > 1 && col_word_len > 1 then 
        row_word::col_word::loop t board
      else if row_word_len > 1 && col_word_len <= 1 then row_word::loop t board 
      else if row_word_len <= 1 && col_word_len > 1 then col_word::loop t board
      else loop t board
  in loop first_letters board

(** [are_words_valid word_list] returns whether all words in word_list are in
    the English dictionary *)
let rec are_words_valid word_list dict = 
  match word_list with 
  | h::t -> (Dictionary.Dict.mem (String.uppercase_ascii h) dict) 
            && are_words_valid t dict
  |[] -> true

let is_valid_board (board:t) = 
  let word_list = word_list_from_board board in
  let row_num = List.length (get_board_row 1 board) in  
  let col_num = List.length (get_board_col 1 board) in 
  let center_pos = (col_num/2 + 1, row_num/2 +1 ) in
  (are_words_valid word_list dict) && (are_connected_to_center center_pos board)

(*Returns a string representation of a board_square that looks good for printing.*)
let bsquare_tostring bsquare = 
  match bsquare.occ with
  | Some ti -> ("|" ^ (String.make 1 ti))
  | None -> "|#"

(* Prints the given list of board_squares IN ORDER to the console. *)
let rec print_ordered_row = function
  | h::t -> print_string (bsquare_tostring h); print_ordered_row t
  | _ -> print_endline "|"

(*Prints each of the rows on the board.
  TODO: Ensure that the rows from [get_board_row] are ORDERED left to right.*)
let rec print_board_helper board n rowcounter: unit = 
  if rowcounter > n then ()
  else (print_ordered_row (get_board_row rowcounter board); 
        print_board_helper board n (rowcounter + 1); ())

(*Main functionality is in helper. This simply sets the row counter to 1 and 
  lets the helper do the main work. *)
let print_board board : unit = 
  let n = (List.length (get_board_row 1 board)) in 
  print_board_helper board n 1

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

let rec merge_boards (board1:board_square list) (board2:t) : t  = 
  match board1 with 
  |[] -> board2
  |{pos = _ ; occ= None}::t -> board2
  |{pos = p ; occ= Some tile}::t -> 
    if (get_square p board2) = {pos = p; occ = None}  then 
      merge_boards t (set_square tile p board2) else raise (InvalidPos p)
