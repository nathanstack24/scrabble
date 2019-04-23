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
exception NotConnected
exception BadWord
exception OneLetter
exception InvalidChar

(**Positions with premium bonuses when words are played on them. *)
type premiums = { dl: position list; tl: position list; 
                  dw: position list; tw: position list; }

let premiums = 
  { dl = [(1,4);(1,12);(3,7);(3,9);(4,1);(4,8);(4,15);(7,3);
          (7,7);(7,9);(7,13);(8,4); (8,12); (9,3);(9,7);(9,9);
          (9,13);(12,1);(12,8);(12,15);(13,7);(13,9);(15,4);(15,12)];
    tl = [(2,6);(2,10);(6,2);(6,6);(6,10);(6,14);(10,2);(10,6);
          (10,10);(10,14);(14,6);(14,10);];
    dw = [(2,2);(3,3);(4,4);(5,5);(8,8);
          (2,14);(3,13);(4,12);(5,11);
          (11,11);(12,12);(13,13);(14,14);
          (11,5);(12,4);(13,3);(3,7);(14,2)];
    tw = [(1,1);(1,15);(15,1);(15,15)]
  }

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

let make_board_square (c:char option) (row:int) (col:int) : board_square = 
  {pos=(row,col); occ=c}

let set_square (tile:tile) (pos:position) (board:t): t = 
  set_square_helper [] tile pos board


let get_neighbors (p:position) (pos_list: position list) = 
  List.filter (fun p_neigh -> 
      (snd p = snd p_neigh && 
       (fst p = fst p_neigh + 1 || fst p = fst p_neigh - 1)) ||
      (fst p = fst p_neigh && 
       (snd p = snd p_neigh + 1 || snd p = snd p_neigh - 1 ))) pos_list

(** [connected_to_center center_pos board] Returns a list of all of the occupied
    board_squares with a path from the center center*)
let connected_to_center (center_pos:position) (board:t)=
  let rec bfs (connected:position list) (visited:position list) (queue:board_square list) (pos_list:position list) board= 
    match queue with 
    |{pos = p; occ = Some tile}::t ->
      (* Check that there is > 0 elements within 1 square of the square
         being visited*)
      let neigh_list = List.filter (fun el -> (List.mem el visited) = false) (get_neighbors p pos_list) in 
      let neigh_squares = List.map (fun pos -> get_square pos board) neigh_list in 
      bfs (neigh_list @ connected) (p::visited) (neigh_squares@t) pos_list board
    |{pos = p; occ = _ }::t -> bfs connected (p::visited) t pos_list board
    |[] -> List.filter (fun pos -> get_occ (get_square pos board) <> None) connected
  in bfs (center_pos::[]) [] ((get_square center_pos board)::[]) 
    (List.map (fun square -> get_pos square) board) board

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

let first_letter_squares_row (board:t) = 
  let rec loop (unvisited:board_square list) (acc:position list) (board:t)= 
    let col_num = List.length (get_board_row 1 board) in
    match unvisited with 
    |[] -> acc
    |{pos=p; occ = None}::t -> loop t acc board
    |{pos=p; occ = Some tile}::t when fst p = 1 -> 
      let (right:position) = ((fst p) + 1, snd p) in 
      if (get_occ (get_square right board) <> None) then loop t (p::acc) board
      else loop t acc board
    |{pos=p; occ = Some tile}::t when fst p = col_num -> loop t acc board
    |{pos=p; occ = Some tile}::t ->  
      let (left:position) = ((fst p) - 1, snd p) in 
      let (right:position) = ((fst p) + 1, snd p) in
      if ((get_occ(get_square left board) = None) && 
          (get_occ (get_square right board) <> None)) then
        loop t (p::acc) board else loop t acc board in 
  loop board [] board 

(** [first_letter_squares_col board] returns the positions of the first letters
    of each column word on the board*)
let first_letter_squares_col (board:t) = 
  let rec loop (unvisited:board_square list) (acc:position list) (board:t)= 
    let row_num = List.length (get_board_col 1 board) in
    match unvisited with 
    |[] -> acc
    |{pos=p; occ = None}::t -> loop t acc board
    |{pos=p; occ = Some tile}::t when snd p = row_num -> 
      let (down:position) = (fst p, (snd p) - 1) in 
      if (get_occ (get_square down board) <> None) then loop t (p::acc) board
      else loop t acc board
    |{pos=p; occ = Some tile}::t when snd p = 1 -> loop t acc board
    |{pos=p; occ = Some tile}::t ->  
      let (up:position) = (fst p, (snd p)+1) in 
      let (down:position) = (fst p, (snd p) - 1) in 
      if ((get_occ (get_square up board) = None) &&
          (get_occ (get_square down board) <> None))
      then loop t (p::acc) board else loop t acc board in 
  loop board [] board 

(** comparison function for sorting squares by the x coordinate of their 
    position*)
let comp_squares_x square1 square2 =
  if get_x square1 = get_x square2 then 0
  else if get_x square1 > get_x square2 then 1 else -1

(** comparison function for sorting squares by the y coordinate of their 
    position*)
let comp_squares_y square1 square2 =
  if get_y square1 = get_y square2 then 0
  else if get_y square1 > get_y square2 then -1 else 1

(** [find_word_row pos board] finds the horizontal word starting at [pos] of 
    [board]*)
let find_word_row (first_letter_pos:position) (board:t)= 
  let row_ind = snd first_letter_pos in
  let sorted_row = List.sort comp_squares_x (get_board_row row_ind board) in 
  let col_ind = fst first_letter_pos in
  let rec loop lst c word = 
    match lst with 
    |{pos = (x, _); occ = _}::t when x < c -> loop t c word
    |{pos = (x, _); occ = Some tile}::t -> loop t c (word^(String.make 1 tile))
    |{pos = (x, _); occ = None}::t -> word
    |[] -> word 
  in loop sorted_row col_ind ""

(** [find_word_col pos board] finds the vertical word starting at [pos] of 
    [board]*)
let find_word_column (first_letter_pos:position) (board:t)= 
  let col_ind = fst first_letter_pos in
  let sorted_col = List.sort comp_squares_y (get_board_col col_ind board) in 
  let row_ind = snd first_letter_pos in
  let rec loop lst r word = 
    match lst with 
    |{pos = (_, x); occ = _}::t when x > r -> loop t r word
    |{pos = (_, x); occ = Some tile}::t -> loop t r (word^(String.make 1 tile))
    |{pos = (_, x); occ = None}::t -> word
    |[] -> word 
  in loop sorted_col row_ind ""

(** [print_pos] prints the position [pos] to the terminal*)
let print_pos (pos:position) = 
  (print_int (fst pos)); print_string " , ";  (print_int (snd pos))

(** prints a list of positions*)
let rec print_pos_list = function 
    [] -> ()
  | e::l -> print_pos e ; print_string " " ; print_pos_list l

(** [remove_dups list acc] returns [list] with no duplicate entries*)
let rec remove_dups lst acc = 
  match lst with
  |[] -> acc
  |h::t -> if (List.mem h acc) = false then remove_dups t (h::acc) 
    else remove_dups t acc

(** [word_list_from_board_col board] returns a list of all the vertical words on
    [board]*)
let word_list_from_board_col (board:t) = 
  let first_letters = remove_dups (first_letter_squares_col board) [] in
  let rec loop pos_list board = 
    match pos_list with
    |[] -> []
    |h::t ->
      let col_word = find_word_column h board in 
      let col_word_len = String.length col_word in 
      if col_word_len > 1 then 
        col_word::loop t board
      else loop t board
  in loop first_letters board

(** [word_list_from_board_row board] returns a list of all the horizontal words 
    on [board]*)
let word_list_from_board_row (board:t) = 
  let first_letters = remove_dups (first_letter_squares_row board) [] in
  let rec loop pos_list board = 
    match pos_list with
    |[] -> []
    |h::t ->
      let row_word = find_word_row h board in 
      let row_word_len = String.length row_word in 
      if row_word_len > 1 then 
        row_word::loop t board
      else loop t board
  in loop first_letters board

(** [are_words_valid word_list] returns whether all words in word_list are in
    the English dictionary *)
let rec are_words_valid word_list dict = 
  match word_list with 
  |h::t -> if (Dict.mem (String.uppercase_ascii h) dict) = false then false 
    else are_words_valid t dict
  |[] -> true


let check_word_list_from_board_col (board:t) = 
  let first_letters = first_letter_squares_col board in
  let rec loop pos_list board = 
    match pos_list with
    |[] -> true
    |h::t ->
      let col_word = find_word_column h board in 
      if (Dict.mem (String.uppercase_ascii col_word) dict) = false then false 
      else loop t board
  in loop first_letters board

let check_word_list_from_board_row (board:t) = 
  let first_letters = remove_dups (first_letter_squares_row board) [] in
  let rec loop pos_list board = 
    match pos_list with
    |[] -> true
    |h::t ->
      let row_word = find_word_row h board in 
      if (Dict.mem (String.uppercase_ascii row_word) dict) = false then false 
      else loop t board
  in loop first_letters board

let check_words (board) = 
  check_word_list_from_board_col board && check_word_list_from_board_row board

let is_valid_board (board:t) = 
  let word_list = (word_list_from_board_row board) @ (word_list_from_board_col board) in
  let row_num = List.length (get_board_row 1 board) in  
  let col_num = List.length (get_board_col 1 board) in 
  let center_pos = (col_num/2 + 1, row_num/2 +1 ) in
  if (are_words_valid word_list dict) then 
    (if (are_connected_to_center center_pos board) then 
       (if (List.length word_list = 0) && 
           (get_occ (get_square center_pos board)) <> None then raise OneLetter 
        else true)
     else raise NotConnected)
  else raise BadWord

(** [bsquare_tostring bsquare] Returns a string representation of a board_square 
    that looks good for printing for the GUI.*)
let bsquare_tostring bsquare = 
  match bsquare.occ with
  | Some ti -> (String.make 1 ti)
  | None -> if bsquare.pos<>(8,8) then "#" else "*"

(*Gets the color the tile should print at.*)
let get_bsquare_color bsquare = 
  let pos = bsquare.pos in
  if List.mem pos premiums.dl then [ANSITerminal.on_cyan;ANSITerminal.black;Bold]
  else if List.mem pos premiums.tl  then [ANSITerminal.on_blue;ANSITerminal.black;Bold]
  else if List.mem pos premiums.dw then [ANSITerminal.on_magenta;ANSITerminal.black;Bold]
  else if List.mem pos premiums.tw then [ANSITerminal.on_red;ANSITerminal.black;Bold]
  else [ANSITerminal.white;ANSITerminal.black]

(* Prints the given list of board_squares IN ORDER to the console. *)
let rec print_ordered_row = function
  | h::t -> print_string "|"; ANSITerminal.(print_string (get_bsquare_color h) 
                                              (bsquare_tostring h)); print_ordered_row t
  | _ -> print_endline "|"

(** [print_row_num num] prints the given row number to the console. Called 
  * before printing each row of the Scrabble board. *)
let print_row_num num = 
  if (num < 10) then print_string (" " ^(string_of_int num))
  else print_string (string_of_int num)

(** [print_col_num num stop] prints the given column number [num] to the console 
  * unless the stop condition [num]=[stop] is met. Called after the entire 
  * Scrabble board has been printed. *)
let rec print_col_num (num:int) (stop:int) = 
  if (num=stop) then print_newline () else
    print_string ((string_of_int num) ^ " ");
  print_col_num (num+1) stop

(** [print_board_helper board rowcounter] prints each of the rows on the Scrabble
  * board [board] with corresponding row identifier [row_counter]. When [rowcounter]
  * reaches zero, all rows of the Scrabble board have been printed. *)
let rec print_board_helper board rowcounter: unit = 
  if rowcounter = 0 then ()
  else (
    print_row_num rowcounter;
    print_ordered_row (List.sort comp_squares_x (get_board_row rowcounter board)); 
    print_board_helper board (rowcounter - 1); ())

(** [make_x_coord_string col_num] returns the string of integers to be printed
    at the bottom of a board with [col_num] columns*)
let rec make_x_coord_string col_num = 
  let col_str = (string_of_int col_num) in 
  let last_dig = String.get col_str ((String.length col_str) - 1) in 
  if col_num = 0 then "" else  make_x_coord_string (col_num-1)^ " " ^ (String.make 1 last_dig)

(*Main functionality is in helper. This simply sets the row counter to 1 and 
  lets the helper do the main work. *)
let print_board board : unit = 
  ANSITerminal.(print_string [red] "The board:\n");
  let n = (List.length (get_board_row 1 board)) in 
  print_board_helper board n;
  print_string ("  " ^ (make_x_coord_string n))


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

let make_pos col row:position = (col, row)

(** [i -- j] is the list of integers from [i] to [j], inclusive.
    Tail recursive. *)
let (--) (i : int) (j : int) : int list =
  let rec from i j l =
    if i>j then l
    else from i (j-1) (j::l)
  in from i j []

let make_tile (c:char) : tile = 
  let code = (Char.code c) in 
  let range = 65 -- 90 in 
  if (List.mem code range) then c else raise InvalidChar

let rec get_word_score (word:string) (n:int) : int = 
  if n = 0 then 0 else 
    let curr_char = String.get word (n-1) in
    let char_score = Values.find curr_char tile_values in 
    char_score + get_word_score word (n-1)

(** get_word_difference returns the words that are in list2 that are not in 
    list1*)
let get_word_difference wordlist1 wordlist2 = 
  List.filter (fun w2-> List.mem w2 wordlist1 = false) wordlist2

let get_board_score (old_board:t) (new_board:t)= 
  let old_words = (word_list_from_board_row old_board) @ 
                  (word_list_from_board_col old_board) in 
  let all_words = (word_list_from_board_row new_board) @ 
                  (word_list_from_board_col new_board) in 
  let new_words = get_word_difference old_words all_words in
  let rec loop words = 
    match words with 
    |h::t -> (get_word_score h (String.length h)) + loop t
    |[] -> 0
  in loop new_words

let get_board_word_diff old_board new_board =
  let old_words = (word_list_from_board_row old_board) @ 
                  (word_list_from_board_col old_board) in 
  let all_words = (word_list_from_board_row new_board) @ 
                  (word_list_from_board_col new_board) in 
  let new_words = get_word_difference old_words all_words in
  let rec loop words = 
    match words with 
    |h::t -> h :: (loop t)
    |[] -> []
  in loop new_words

let print_tile (tile:tile) = print_char tile

let get_row_num (board:t) :int = List.length (get_board_row 1 board)

let get_col_num (board:t) :int = List.length (get_board_col 1 board)

let get_board_positions board = List.map (fun bs -> get_pos bs) board

let get_x_pos pos = fst pos

let get_y_pos pos = snd pos