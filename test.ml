open OUnit2
open Board
open Command
open State

(********************************************************************
   Here are some helper functions for your testing of set-like lists. 
 ********************************************************************)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
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
let pos21 = make_pos 2 1
let pos22 = make_pos 2 2
let a = make_tile 'A'
let b = make_tile 'B'
let c = make_tile 'C'
let board5x5 = new_board 5
let square12A = set_occ square12emp a
let board2x2a = set_square a pos12 board2x2
(*
let make_get_x_tests  
    (name : string) 
    (square : Board.board_square) 
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_x square))

let make_get_y_tests  
    (name : string) 
    (square : Board.board_square) 
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_y square))
*)
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
(*
let make_get_occ_tests  
    (name : string) 
    (square : Board.board_square) 
    (expected_output : tile option) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_occ square))

let make_set_occ_tests  
    (name : string) 
    (square : Board.board_square)
    (tile : tile) 
    (expected_output : board_square) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (set_occ square tile))

let make_dict_tests  
    (name : string) 
    (square : Board.board_square)
    (tile : tile) 
    (expected_output : board_square) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (set_occ square tile)) *)

let board_tests = [
  make_get_pos_tests "get pos 1, 1" square11emp pos11;
  make_get_pos_tests "get pos 1, 2" square12emp pos12;

  make_get_square_tests "get square empty 1 1" pos11 board2x2 square11emp;
  make_get_square_tests "get square empty 1 2" pos12 board2x2 square12emp;
  make_get_square_tests "get square 1 2 A" pos12 board2x2a square12A;
]

let dictionary_tests = [


]


let suite = "Scrabble test suite" >::: List.flatten [
    board_tests;
  ]

let _ = run_test_tt_main suite
