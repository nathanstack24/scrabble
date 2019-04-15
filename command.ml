(* Note: You may introduce new code anywhere in this file. *) 

type object_phrase = string list

type item = string list

type verbal_phrase = string list

type command = 
  | Quit
  | Place
  | Remove 
  | Endturn
  | Score

exception Empty
exception Malformed

(** [clean_str] removes any excess spaces. *)
let rec clean_str (lst:string list) : (string list) =
  match lst with
  | [] -> []
  | h::t -> if String.equal h "" then clean_str t
    else h::clean_str t

(** [parse] performs the appropriate command given the user-inputed string. If 
    the command is empty, we will not do anything. If the command is "quit," we
    will quit, etc.*)
let parse str : command =
  let cmd = (clean_str (String.split_on_char ' ' (String.trim str))) in
  if List.length cmd = 0 then raise Empty else 
    match cmd with 
    | "quit"::t -> Quit
    | _ > Place

(*
  else if String.equal (List.hd cmd) "quit" then
    if (List.length cmd <> 1) then raise Malformed else Quit
  else if String.equal (List.hd cmd) "place" then
    if (List.length cmd = 1) then raise Malformed else Place [(List.tl cmd), 
  else if String.equal (List.hd cmd) "endturn" then 
    if (List.length cmd = 1) then raise Malformed else Remove (List.tl cmd)
  else if String.equal (List.hd cmd) "remove" then
    if (List.length cmd = 1) then raise Malformed else endturn (List.tl cmd)
  else if String.equal (List.hd cmd) "score" then
    if (List.length cmd <> 1) then raise Malformed else Score
  else raise Malformed
   *)