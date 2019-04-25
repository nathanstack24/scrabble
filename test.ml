open OUnit2
open Board
open Command
open State
open Dictionary

(** [cmp_unordered_lists lst1 lst2] compares two lists to see whether
    they are equivalent unordered lists.  That means checking that 
    they must contain the same elements, though not necessarily in the same 
    order. *)
let cmp_unordered_lists lst1 lst2 =
  let uniq1 = List.sort compare lst1 in
  let uniq2 = List.sort compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2


(* BOARD test cases*)

let board2x2 = new_board 2
let square11emp = make_board_square None 1 1
let square12emp = make_board_square None 1 2
let square21emp = make_board_square None 2 1
let square22emp = make_board_square None 2 2
let pos11 = make_pos 1 1
let pos12 = make_pos 1 2
let pos13 = make_pos 1 3
let pos21 = make_pos 2 1
let pos22 = make_pos 2 2
let pos33 = make_pos 3 3
let pos43 = make_pos 4 3
let pos53 = make_pos 5 3
let pos32 = make_pos 3 2
let pos31 = make_pos 3 1
let pos42 = make_pos 4 2
let pos52 = make_pos 5 2
let pos65 = make_pos 6 5
let pos01 = make_pos 0 1
let posneg = make_pos (-5) 2
let a = make_tile 'A'
let b = make_tile 'B'
let c = make_tile 'C'
let e = make_tile 'E'
let t = make_tile 'T'
let board5x5 = new_board 5 (* empty 5x5 board*)
let square12A = set_occ square12emp a
let board2x2a = set_square a pos12 board2x2
let board5x5c = set_square c pos33 board5x5 (* 5x5 board (3,3):c *)
let board5x5ca = set_square a pos43 board5x5c (* 5x5 board (3,3):c, (4,3):a *)
(* 5x5 board (3,3):c, (4,3):a, (5,3):b *)
let board5x5cab = set_square b pos53 board5x5ca 
(* 5x5 board (1,1):a, (1,2):b, (3,3):c, (4,3):a, (5,3):b *)
let board_disc = set_square b pos11 (set_square a pos12 board5x5cab) 
let square43a = make_board_square (Some 'A') 4 3
let boardvertca = set_square a pos32 board5x5c
let boardvertcab = set_square b pos31 board5x5c
let board5x5cabb = set_square b pos42 board5x5cab
let board5x5cabbe = set_square e pos52 board5x5cabb

let rec make_pos_list x y n= 
  if y = 0 then []
  else if x = 0 then  make_pos_list n (y-1) n
  else (make_pos x y) :: (make_pos_list (x-1) y n)

let make_get_pos_tests  
    (name : string) 
    (square : Board.board_square) 
    (expected_output : position) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_pos square))

let make_get_square_tests  
    (name : string) 
    (pos : Board.position)
    (board: Board.t) 
    (expected_output : board_square) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_square pos board))

let make_merge_boards_tests  
    (name : string) 
    (sq_list : Board.board_square list)
    (board: Board.t) 
    (expected_output : Board.t) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (merge_boards sq_list board))

let make_set_square_exn_tests
    (name: string)
    (t : tile)
    (p : position)
    (b : Board.t)
    (exn : exn) : test = 
  name >:: (fun _ -> 
      assert_raises exn (fun () -> set_square t p b)) 

let make_merge_boards_tests  
    (name : string) 
    (sq_list : Board.board_square list)
    (board: Board.t) 
    (expected_output : Board.t) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (merge_boards sq_list board))

let make_board_valid_tests  
    (name : string) 
    (board: Board.t) 
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (is_valid_board board))

let make_bad_boards_tests
    (name : string) 
    (board: Board.t) 
    (exn: exn): test = 
  name >:: (fun _ -> 
      assert_raises exn (fun () -> is_valid_board board))

let make_get_board_score_tests
    (name : string) 
    (old_board: Board.t) 
    (new_board: Board.t) 
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_board_score old_board new_board))

let make_board_word_diff_tests
    (name : string) 
    (old_board: Board.t) 
    (new_board: Board.t) 
    (expected_output : string list) : test = 
  name >:: (fun _ -> 
      assert_equal true (cmp_unordered_lists
                           expected_output 
                           (get_board_word_diff old_board new_board)))

let make_possible_words_tests
    (name:string)
    (board:Board.t)
    (inv:Board.tile list)
    (expected_output: string list) : test = 
  name >:: (fun _ -> 
      assert_equal true (cmp_unordered_lists
                           expected_output 
                           (possible_words_dict inv board)))

let board_tests = [
  make_get_pos_tests "get pos 1, 1" square11emp pos11;
  make_get_pos_tests "get pos 1, 2" square12emp pos12;

  make_get_square_tests "get square empty 1 1" pos11 board2x2 square11emp;
  make_get_square_tests "get square empty 1 2" pos12 board2x2 square12emp;
  make_get_square_tests "get square 1 2 A" pos12 board2x2a square12A;

  make_set_square_exn_tests "set square 6 5 exn" a pos65 board5x5 
    (InvalidPos pos65);
  make_set_square_exn_tests "set square 0 1 exn" a pos01 board5x5 
    (InvalidPos pos01);
  make_set_square_exn_tests "set square -5 2 exn" a posneg board5x5 
    (InvalidPos posneg);

  make_merge_boards_tests "merge boards 5x5 c" ([]) board5x5c board5x5c;
  make_merge_boards_tests "merge boards 5x5 ca" 
    (square43a::[]) board5x5c board5x5ca;

  make_board_valid_tests "valid board 5x5 cab" board5x5cab true;
  make_bad_boards_tests "invalid board 5x5 ca" board5x5ca BadWord;
  make_bad_boards_tests "invalid board 5x5 disconnected" board_disc NotConnected;
  make_bad_boards_tests "invalid board 5x5 vertical ca" boardvertca BadWord;
  make_bad_boards_tests "invalid board 5x5 c" board5x5c OneLetter;

  make_get_board_score_tests "5x5 cab board score" board5x5 board5x5cab 14;
  make_get_board_score_tests "5x5 empt -> cab be be board score" 
    board5x5 board5x5cabbe 26;
  make_get_board_score_tests "5x5 cab -> ab be be board score" 
    board5x5cab board5x5cabbe 12;

  make_board_word_diff_tests "5x5 empt -> cab word diff" board5x5 board5x5cab 
    ["CAB"];
  make_board_word_diff_tests "5x5 empt -> cab ab be be word diff" board5x5 
    board5x5cabbe ["CAB"; "AB"; "BE"; "BE"];
  make_board_word_diff_tests "5x5 cab -> cab ab be be word diff" 
    board5x5cab board5x5cabbe
    ["AB"; "BE"; "BE"];

  make_possible_words_tests "possible words 1" board5x5 [a;b] ["AB"; "BA"];
  make_possible_words_tests "possible words 2" board5x5 [c;a;b] 
    ["AB"; "BA"; "CAB";"BAC"];
  make_possible_words_tests "possible words 3" board5x5c [a;b] 
    ["AB"; "BA"; "CAB"; "BAC"];
  make_possible_words_tests "possible words 4" board5x5cab [a;b] 
    ["AB"; "BA"; "CAB";"BAC";"ABA"; "ABAC"; "ABB";"ABBA";"AA";
     "BABA";"CABA";"CAA";"BAA"];

  "col_num 5" >:: (fun _ -> assert_equal 5 (get_col_num board5x5c));
  "col_num 2" >:: (fun _ -> assert_equal 2 (get_col_num board2x2));
  "row_num 5" >:: (fun _ -> assert_equal 5 (get_row_num board5x5c));
  "row_num 2" >:: (fun _ -> assert_equal 2 (get_row_num board2x2));

  "pos_list" >:: (fun _ -> assert_equal true 
                     (cmp_unordered_lists (make_pos_list 5 5 5) 
                        (get_board_positions board5x5)))
]

(* state test cases*)
let state_init = init_state 2 0
let state_2 = State.end_turn state_init
let state_3 = State.end_turn state_2

let bad_state_1 = fun () -> init_state 1 0
let bad_state_0 = fun () -> init_state 0 0 
let bad_state_5 = fun () -> init_state 5 0 
let bad_state_neg = fun () -> init_state (-1) 0

let make_bad_state_init_tests
    (name : string) 
    (func: unit -> State.t) = 
  name >:: 
  (fun _ -> assert_raises (InvalidNumPlayers) (func))

let make_get_curr_player_tests 
    (name : string) 
    (state: State.t) 
    (exp: int): test = 
  name >:: (fun _ -> 
      assert_equal exp (get_curr_player_id state))

let state_tests = [
  make_bad_state_init_tests "state with one player" bad_state_1;
  make_bad_state_init_tests "state with zero players" bad_state_0;
  make_bad_state_init_tests "state with negative players" bad_state_neg;
  make_bad_state_init_tests "state with more than 4 players" bad_state_5;

  make_get_curr_player_tests "init player id" state_init 1;
  make_get_curr_player_tests "endturn player id" state_2 2;
  make_get_curr_player_tests "2*endturn player id" state_3 1;

  "get_scores test" >:: (fun _ -> 
      assert_equal [(1,0);(2,0)] (get_scores state_3));
]

(* scrabble dictionary test cases *)
let dict = Dictionary.dict

let make_dict_tests  
    (name : string) 
    (word: string)
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Hashtbl.mem dict (String.uppercase_ascii word))) 

let dictionary_tests = [
  make_dict_tests "AA test" "AA" true;
  make_dict_tests "AAH test" "AAH" true;
  make_dict_tests "AAAA test" "AAAA" false;
  make_dict_tests "DOG test" "DOG" true;
  make_dict_tests "ASDFGJKL test" "ASDFGJKL" false;
  make_dict_tests "ZZZ test" "ZZZ" true;
  make_dict_tests "ZZZS test" "ZZZS" true;
  make_dict_tests "ZZZZ test" "ZZZZ" false;
]

(* tile values tests*)

let make_tile_value_tests  
    (name : string) 
    (character: char)
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Values.find character tile_values))

let tile_values_tests = [
  make_tile_value_tests "A" 'A' 1;
  make_tile_value_tests "B" 'B' 3;
  make_tile_value_tests "D" 'D' 2;
  make_tile_value_tests "F" 'F' 4;
  make_tile_value_tests "J" 'J' 8;
  make_tile_value_tests "K" 'K' 5;
  make_tile_value_tests "Q" 'Q' 10;
  make_tile_value_tests "Z" 'Z' 10;
]

let suite = "Scrabble test suite" >::: List.flatten [
    board_tests;
    dictionary_tests;
    tile_values_tests;
    state_tests;
  ]

let _ = run_test_tt_main suite
