open Board

type command = 
  | Quit
  | Place of tile*position
  | Remove of Board.position
  | Endturn
  | Score
  | Help
  | Perfect 

exception Empty
exception Malformed

let rec clean_str (lst:string list) : (string list) =
  match lst with
  | [] -> []
  | h::t -> if String.equal h "" then clean_str t
    else h::clean_str t

let parse str : command =
  let cmd = (clean_str (String.split_on_char ' ' (String.trim str))) in
  if List.length cmd = 0 then raise Empty else 
    match cmd with 
    | "quit"::t -> Quit
    | "place"::c::row::col::[] 
      -> Place ((Board.make_tile (String.get c 0) ), 
                (Board.make_pos (int_of_string row) (int_of_string col)))
    | "remove"::row::col::[] -> Remove (Board.make_pos (int_of_string row) (int_of_string col))
    | "endturn"::[] -> Endturn
    | "score"::[] -> Score
    | "help"::[] -> Help
    | "perfect"::[] -> Perfect
    | _ -> raise Malformed