open Dictionary

type tile = char

type position = int*int

type board_square = {
  pos : position;
  occ : tile option
}

type t = board_square list

exception Occupied of board_square
exception InvalidPos of position
exception NotConnected
exception BadWord
exception OneLetter
exception InvalidChar


(** [premiums_type] is a variant, each element of which describes the possible
  * bonus effect that a position on the Scrabble board might have. *)
type premium_type = 
  | DL
  | DW
  | TL
  | TW
  | No_Premium

(** [add_to_premiums premiums premium lst] adds each position in [lst]
  * to hash table [premiums] with corresponding premium type [premium] *)
let rec add_to_premiums (premiums: (position, premium_type) Hashtbl.t)
    (premium:premium_type) (lst: position list) = 
  match lst with 
  | [] -> ()
  | h::t -> Hashtbl.add premiums h premium; 
    add_to_premiums premiums premium t


(** [dl_list] is a list of all of the positions of double letter squares*)
let dl_list = [(1,4);(1,12);(3,7);(3,9);(4,1);(4,8);(4,15);(7,3);
               (7,7);(7,9);(7,13);(8,4); (8,12); (9,3);(9,7);(9,9);
               (9,13);(12,1);(12,8);(12,15);(13,7);(13,9);(15,4);(15,12)]

(** [tl_list] is a list of all of the positions of triple letter squares*)
let tl_list = [(2,6);(2,10);(6,2);(6,6);(6,10);(6,14);(10,2);(10,6);
               (10,10);(10,14);(14,6);(14,10);]

(** [dw_list] is a list of all of the positions of double word squares*)
let dw_list = [(2,2);(3,3);(4,4);(5,5);(8,8); (2,14);(3,13);(4,12);(5,11);
               (11,11);(12,12);(13,13);(14,14);(11,5);(12,4);(13,3);(3,7);
               (14,2)]

(** [tw_list] is a list of all of the positions of double word squares*)
let tw_list = [(1,1);(1,15);(15,1);(15,15)]

let print_tuple (tup: position) =
  print_string ("(" ^ (string_of_int (fst tup)) ^ ", " ^ 
                (string_of_int (snd tup)) ^ ")")

let print_premium_type (p_type:premium_type) =
  let s = 
    match p_type with
    | TW -> "TW"
    | DW -> "DW"
    | DL -> "DL"
    | TL -> "TL"
    | No_Premium -> "None" in 
  print_string s

(** [premiums] is a hash table where each premium tile position maps to its
  * associated multiplier (triple letter, triple word, double letter, or 
  * double word). *)
let premiums : (position, premium_type) Hashtbl.t = 
  let premiums_table = Hashtbl.create 50 in 
  add_to_premiums premiums_table DL dl_list;
  add_to_premiums premiums_table TL tl_list;
  add_to_premiums premiums_table DW dw_list;
  add_to_premiums premiums_table TW tw_list;
  premiums_table

let rec remove_tiles_from_premiums (b_squares: board_square list) = 
  match b_squares with 
  | [] -> () 
  | h::t -> Hashtbl.remove premiums h.pos; remove_tiles_from_premiums t

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
  let rec bfs (connected:position list) (visited:position list) 
      (queue:board_square list) (pos_list:position list) board= 
    match queue with 
    |{pos = p; occ = Some tile}::t ->
      (* Check that there is > 0 elements within 1 square of the square
         being visited*)
      let neigh_list = List.filter (fun el -> (List.mem el visited) = false)
          (get_neighbors p pos_list) in 
      let neigh_squares = List.map
          (fun pos -> get_square pos board) neigh_list in 
      bfs (neigh_list @ connected) (p::visited) (neigh_squares@t) pos_list board
    |{pos = p; occ = _ }::t -> bfs connected (p::visited) t pos_list board
    |[] -> List.filter 
             (fun pos -> get_occ (get_square pos board) <> None) connected
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

(** [first_letter_squares_row board] returns the positions of the first letters
    of each row word on the board*)
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

(** [comp_squares_x square1 square2] returns 0 if two board squares have the 
    same x, 1 if square1>square2 x and -1 otherwise*)
let comp_squares_x square1 square2 =
  if get_x square1 = get_x square2 then 0
  else if get_x square1 > get_x square2 then 1 else -1

(** [comp_squares_y square1 square2] returns 0 if two board squares have the 
    same y, 1 if square1>square2 y and -1 otherwise*)
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


(** [find_premium_val p] returns the premium type associated with position [p],
  * or No_Premium if position [p] is not associated with any bonus, or if the
  * bonus associated with that type was already used. *)
let find_premium_val (p:position) : premium_type =
  try Hashtbl.find premiums p with 
  | Not_found -> No_Premium

(** [find_word_mul_row pos board] finds the horizontal word starting at [pos] of 
    [board] and its associated multiplier  *)
let find_word_mul_row (first_letter_pos:position) (board:t) = 
  let row_ind = snd first_letter_pos in
  let sorted_row = List.sort comp_squares_x (get_board_row row_ind board) in 
  let col_ind = fst first_letter_pos in
  let rec loop lst c acc = 
    match lst with 
    |{pos = (x, _); occ = _}::t when x < c -> loop t c acc
    |{pos = p; occ = Some tile}::t -> 
      let bonus = find_premium_val p in 
      if (bonus=DL) then
        loop t c ((tile,"dl")::acc)
      else if (bonus=DW) then
        loop t c ((tile, "dw")::acc)
      else if (bonus=TL) then
        loop t c ((tile, "tl")::acc)
      else if (bonus=TW) then 
        loop t c ((tile, "tw")::acc)
      else 
        loop t c ((tile, "")::acc)
    |{pos = (x, _); occ = None}::t -> acc
    |[] -> acc 
  in loop sorted_row col_ind []

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


(** [find_word_mul_col pos board] finds the vertical word starting at [pos] of 
    [board]*)
let find_word_mul_column (first_letter_pos:position) (board:t)= 
  let col_ind = fst first_letter_pos in
  let sorted_col = List.sort comp_squares_y (get_board_col col_ind board) in 
  let row_ind = snd first_letter_pos in
  let rec loop lst r acc = 
    match lst with 
    |{pos = (_, x); occ = _}::t when x > r -> loop t r acc
    |{pos = p; occ = Some tile}::t ->    
      let bonus = find_premium_val p in 
      if (bonus=DL) then
        loop t r ((tile,"dl")::acc)
      else if (bonus=DW) then
        loop t r ((tile, "dw")::acc)
      else if (bonus=TL) then
        loop t r ((tile, "tl")::acc)
      else if (bonus=TW) then 
        loop t r ((tile, "tw")::acc)
      else 
        loop t r ((tile, "")::acc)
    |{pos = (_, x); occ = None}::t -> acc
    |[] -> acc 
  in loop sorted_col row_ind []

(** [remove_dups list acc] returns [list] with no duplicate entries *)
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

(** [word_mul_from_board_col board] returns a list of all the vertical words on
    [board] with their associated word_multiplier*)
let word_mul_list_from_board_col (board:t) =
  let first_letters = remove_dups (first_letter_squares_col board) [] in
  let rec loop pos_list board = 
    match pos_list with
    |[] -> []
    |h::t ->
      let col_word_mul = find_word_mul_column h board in 
      let col_word_len = List.length col_word_mul in 
      if col_word_len > 1 then 
        col_word_mul::loop t board
      else loop t board
  in loop first_letters board

(** [word_mul_from_board_row board] returns a list of all the horizontal words 
    on [board] with their associated multiplier*)
let word_mul_list_from_board_row (board:t) = 
  let first_letters = remove_dups (first_letter_squares_row board) [] in
  let rec loop pos_list board = 
    match pos_list with
    |[] -> []
    |h::t ->
      let row_word_mul = find_word_mul_row h board in 
      let row_word_len = List.length row_word_mul in 
      if row_word_len > 1 then 
        row_word_mul::(loop t board)
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
  |h::t -> if (Hashtbl.mem dict (String.uppercase_ascii h)) = false then false 
    else are_words_valid t dict
  |[] -> true

(** [check_word_list_from_board_col board] returns true if all of the vertical
    placed words on a word are in the dictionary and false otherwise*)
let check_word_list_from_board_col (board:t) = 
  let first_letters = first_letter_squares_col board in
  let rec loop pos_list board = 
    match pos_list with
    |[] -> true
    |h::t ->
      let col_word = find_word_column h board in 
      if (Hashtbl.mem dict (String.uppercase_ascii col_word)) = false 
      then false 
      else loop t board
  in loop first_letters board

(** [check_word_list_from_board_col board] returns true if all of the horizontal
    placed words on a word are in the dictionary and false otherwise*)
let check_word_list_from_board_row (board:t) = 
  let first_letters = remove_dups (first_letter_squares_row board) [] in
  let rec loop pos_list board = 
    match pos_list with
    |[] -> true
    |h::t ->
      let row_word = find_word_row h board in 
      if (Hashtbl.mem dict (String.uppercase_ascii row_word)) = false then false 
      else loop t board
  in loop first_letters board

let check_words (board) = 
  check_word_list_from_board_col board && check_word_list_from_board_row board

let is_valid_board (board:t) = 
  let word_list = (word_list_from_board_row board) @ 
                  (word_list_from_board_col board) in
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

(** [get_bsquare_color bsquare] returns a list of [ANSITerminal.style] elements
  * which determine how board square is printed. If [bsquare] is a bonus tile,
  * it is printed with a special background color and it is printed in bold. *)
let get_bsquare_color bsquare = 
  let pos = bsquare.pos in
  let bonus = find_premium_val pos in 
  if bonus=DL then 
    [ANSITerminal.on_cyan;ANSITerminal.black;Bold]
  else if bonus=TL  then 
    [ANSITerminal.on_blue;ANSITerminal.black;Bold]
  else if bonus=DW then 
    [ANSITerminal.on_magenta;ANSITerminal.black;Bold]
  else if bonus=TW then 
    [ANSITerminal.on_red;ANSITerminal.black;Bold]
  else [ANSITerminal.white;ANSITerminal.black]

(** [is_cursor_pos] returns a list of ANSITerminal.style styles used to print
  * on the terminal.  *)
let is_cursor_pos (b:board_square) (cursor:position) : ANSITerminal.style list = 
  if b.pos=cursor then [ANSITerminal.Blink; ANSITerminal.on_yellow] else []

(** [print_tile_details row] prints the tile details corresponding to
  * [row] *)
let print_tile_values (row: int) = 
  ANSITerminal.set_cursor 90 (3 + (15-row));
  match row with 
  | 14 -> print_string "1 point letters: ";
    ANSITerminal.(print_string [green] "A, E, I, O, N, R, T, L, S, U"); 
  | 12 -> print_string "2 point letters: ";
    ANSITerminal.(print_string [green] "D, G"); 
  | 10 -> print_string "3 point letters: ";
    ANSITerminal.(print_string [green] "B, C, M, P"); 
  | 8 -> print_string "4 point letters: ";
    ANSITerminal.(print_string [green] "F, H, V, W, Y"); 
  | 6 ->print_string "5 point letter: ";
    ANSITerminal.(print_string [green] "K"); 
  | 4 ->print_string "8 point letters: ";
    ANSITerminal.(print_string [green] "J, X"); 
  | 2 ->print_string "10 point letters: ";
    ANSITerminal.(print_string [green] "Q, Z"); 
  | _ -> ()

(** [print_legend_details row] prints the legend details corresponding to
  * [row] *)
let print_legend_details (row:int) = 
  ANSITerminal.set_cursor 40 (3 + (15-row));
  match row with 
  | 14 -> ANSITerminal.(print_string [on_yellow] " "); 
    print_string " : the current position of the cursor"
  | 12 ->ANSITerminal.(print_string [on_cyan] " "); 
    print_string " : double letter tile"
  | 10 ->ANSITerminal.(print_string [on_magenta] " "); 
    print_string " : double word tile"
  | 8 ->ANSITerminal.(print_string [on_blue] " "); 
    print_string " : triple letter tile"
  | 6 ->ANSITerminal.(print_string [on_red] " "); 
    print_string " : triple word tile"
  | _ -> ()

(** [print_ordered_row lst cursor row] prints the row of board
  * squares denoted by row number [row]. If this row contains the board square
  * on which the cursor lies (i.e. the board square with position [cursor]), 
  * that boardsquare is printed with a yellow background.
  * *)
let rec print_ordered_row (lst:board_square list) (cursor:position) (row:int) = 
  match lst with 
  | h::t -> print_string "|"; 
    ANSITerminal.print_string ((get_bsquare_color h)@(is_cursor_pos h cursor))
      (bsquare_tostring h); 
    print_ordered_row t cursor row
  | _ -> print_string "|"; print_legend_details row; print_tile_values row;
    print_newline()

(** [print_row_num num] prints the given row number to the console. Called 
  * before printing each row of the Scrabble board. *)
let print_row_num num = 
  if (num < 10) then print_string (" " ^(string_of_int num))
  else print_string (string_of_int num)

(** [print_board_helper board rowcounter] prints each of the rows on the 
  * Scrabble board [board] with corresponding row identifier [row_counter]. 
  * When [rowcounter] reaches zero, all rows of the Scrabble board have been
  * printed. *)
let rec print_board_helper (board:t) (rowcounter:int) (cursor: position) = 
  if rowcounter = 0 then ()
  else (
    print_row_num rowcounter;
    print_ordered_row (List.sort comp_squares_x 
                         (get_board_row rowcounter board)) cursor rowcounter; 
    print_board_helper board (rowcounter - 1) cursor; ())

(** [make_x_coord_string col_num] returns the string of integers to be printed
    at the bottom of a board with [col_num] columns*)
let rec make_x_coord_string col_num = 
  let col_str = (string_of_int col_num) in 
  let last_dig = String.get col_str ((String.length col_str) - 1) in 
  if col_num = 0 then "" else  make_x_coord_string (col_num-1)^ " " ^ 
                               (String.make 1 last_dig)

let print_board (board:t) (cursor:position) : unit = 
  ANSITerminal.(print_string [red] "The board:");
  ANSITerminal.set_cursor 40 2;
  ANSITerminal.(print_string [red] "The legend:");
  ANSITerminal.set_cursor 90 2;
  ANSITerminal.(print_string [red] "The tile values:\n");
  let n = (List.length (get_board_row 1 board)) in 
  print_board_helper board n cursor;
  print_string ("  " ^ (make_x_coord_string n)); 
  let cursor_y = snd (ANSITerminal.pos_cursor ()) in 
  ANSITerminal.set_cursor 40 cursor_y;
  ANSITerminal.(print_string [red] 
                  ("Press enter, type 'help', and then press enter again"^ 
                   " for more details"))

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

let rec get_word_score (word_mul: (tile*string) list ) (n:int) (acc:int*int) = 
  if n = 0 then (fst acc * snd acc) else 
    let curr_char = fst (List.nth word_mul (n-1)) in
    let char_score = Values.find curr_char tile_values in
    let char_mul = snd (List.nth word_mul (n-1)) in
    let word_acc = fst acc in 
    let tot_mul = snd acc in 
    if  char_mul = "dl" then get_word_score word_mul (n-1) 
        ( (2*char_score + word_acc), tot_mul ) 
    else if char_mul = "dw" then get_word_score word_mul (n-1) 
        ((char_score+word_acc), tot_mul*2) 
    else if char_mul = "tl" then get_word_score word_mul (n-1) 
        (3*char_score + word_acc,  tot_mul) 
    else if char_mul = "tw" then get_word_score word_mul (n-1) 
        ((char_score + word_acc), tot_mul * 3) 
    else get_word_score word_mul (n-1) (char_score + word_acc, tot_mul) 

(** [remove_one_of elt lst acc] returns [lst] without one of [elt] if it is in 
    [lst]*)
let rec remove_one_of elt lst acc= 
  match lst with 
  |h::t -> if h = elt then (t@acc) else remove_one_of elt t (h::acc)
  |[] -> acc

(** [get_word_difference lst1 lst2] returns the words that are in list2 that are
    not in list1*)
let rec get_word_difference wordlist1 wordlist2 = 
  match wordlist1 with 
  | h::t -> get_word_difference t (remove_one_of h wordlist2 [])
  | [] -> wordlist2

let get_board_score (old_board:t) (new_board:t)= 
  let old_words = (word_mul_list_from_board_row old_board) @ 
                  (word_mul_list_from_board_col old_board) in 
  let all_words = (word_mul_list_from_board_row new_board) @ 
                  (word_mul_list_from_board_col new_board) in 
  let new_words = get_word_difference old_words all_words in
  let rec loop words = 
    match words with 
    |h::t -> (get_word_score h (List.length h) (0,1)) + loop t
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

(** [bsquares_to_chars_helper acc bs_lst] returns the tiles placed on [bs_list]*)
let rec bsquares_to_chars_helper (acc:char list) = function
  | [] -> acc
  | h::t -> match h.occ with 
    |Some c -> bsquares_to_chars_helper (c::acc) t
    |None -> bsquares_to_chars_helper acc t

(**[bsquares_to_chars board] returns the list of chars placed on [board]*)
let bsquares_to_chars (board:t) :(char list) = 
  bsquares_to_chars_helper [] board

(** [tiles_to_chars tiles] returns the chars associated with each tile in 
    [tiles]*)
let tiles_to_chars (tiles:tile list) : char list = tiles

let possible_words_dict inv board = 
  let clist = List.append (tiles_to_chars inv) (bsquares_to_chars board) in
  let f s _ acc = (if (Dictionary.filter_func clist s) then s::acc else acc) in 
  Hashtbl.fold f dict []

let print_tile (tile:tile) = print_char tile

let get_row_num (board:t) :int = List.length (get_board_row 1 board)

let get_col_num (board:t) :int = List.length (get_board_col 1 board)

let get_board_positions board = List.map (fun bs -> get_pos bs) board

let get_x_pos pos = fst pos

let get_y_pos pos = snd pos

let get_right_pos pos = ((fst pos + 1), snd pos)

let get_down_pos pos = (fst pos, snd pos - 1)
