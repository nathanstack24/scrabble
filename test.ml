open OUnit2
open Board
open Command
open State
open Dictionary


(* BOARD test cases*)

let board2x2 = new_board 2
let square11emp = make_board_square None 1 1
let square12emp = make_board_square None 1 2
let square21emp = make_board_square None 2 1
let square22emp = make_board_square None 2 2
let pos11 = make_pos 1 1
let pos12 = make_pos 1 2
let pos21 = make_pos 2 1
let pos22 = make_pos 2 2
let pos33 = make_pos 3 3
let pos43 = make_pos 4 3
let pos53 = make_pos 5 3
let pos32 = make_pos 3 2
let pos31 = make_pos 3 1
let a = make_tile 'A'
let b = make_tile 'B'
let c = make_tile 'C'
let board5x5 = new_board 5 (* empty 5x5 board*)
let square12A = set_occ square12emp a
let board2x2a = set_square a pos12 board2x2
let board5x5c = set_square c pos33 board5x5 (* 5x5 board (3,3):c *)
let board5x5ca = set_square a pos43 board5x5c (* 5x5 board (3,3):c, (4,3):a *)
let board5x5cab = set_square b pos53 board5x5ca (* 5x5 board (3,3):c, (4,3):a, (5,3):b *)
let board_disc = set_square b pos11 (set_square a pos12 board5x5cab) (* 5x5 board (1,1):a, (1,2):b, (3,3):c, (4,3):a, (5,3):b *)
let square43a = make_board_square (Some 'A') 4 3
let boardvertca = set_square a pos32 board5x5c
let boardvertcab = set_square b pos31 board5x5c


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

let make_board_valid_tests  
    (name : string) 
    (board: Board.t) 
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (is_valid_board board))



let board_tests = [
  make_get_pos_tests "get pos 1, 1" square11emp pos11;
  make_get_pos_tests "get pos 1, 2" square12emp pos12;

  make_get_square_tests "get square empty 1 1" pos11 board2x2 square11emp;
  make_get_square_tests "get square empty 1 2" pos12 board2x2 square12emp;
  make_get_square_tests "get square 1 2 A" pos12 board2x2a square12A;

  make_merge_boards_tests "merge boards 5x5 ca" (square43a::[]) board5x5c board5x5ca;

  make_board_valid_tests "valid board 5x5 cab" board5x5cab true;
  make_board_valid_tests "invalid board 5x5 ca" board5x5ca false;
  make_board_valid_tests "invalid board 5x5 disconnected" board_disc false;
  make_board_valid_tests "invalid board 5x5 vertical ca" boardvertca false;
]

(* scrabble dictionary test cases *)
let dict = create_dictionary

let make_dict_tests  
    (name : string) 
    (word: string)
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Dict.mem word dict)) 


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
  ]

let _ = run_test_tt_main suite
