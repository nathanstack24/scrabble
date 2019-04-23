open Board

type command = 
  | Quit
  | Place of tile*position
  | Remove of Board.position
  | Endturn
  | Score
  | Help
<<<<<<< Updated upstream
=======
  | Board
  | Integer of int
>>>>>>> Stashed changes
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
<<<<<<< Updated upstream
    | "endturn"::[] -> Endturn
    | "score"::[] -> Score
    | "help"::[] -> Help
    | "perfect"::[] -> Perfect
=======
    | "inventory"::t -> Inventory
    | "endturn"::t -> Endturn
    | "score"::t -> Score
    | "help"::t -> Help
    | "board"::t -> Board
    | "perfect"::t -> Perfect
    | h::t -> (try (let i = (int_of_string h) in (Integer i)) with | _ -> raise Malformed)
>>>>>>> Stashed changes
    | _ -> raise Malformed