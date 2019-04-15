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

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1::(h2::t as t') ->
        if n=100 then acc ^ "..."  (* stop printing long list *)
        else loop (n+1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"


(* BOARD test cases*)

let board2x2 = new_board 2

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

let make_get_pos_tests  
    (name : string) 
    (square : Board.board_square) 
    (expected_output : position) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_pos square))

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

