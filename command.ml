open Board

type command = 
  | Quit
  | Place of tile*position
  | Remove of Board.position
  | Inventory
  | Endturn
  | Score
  | Help
  | Board

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
    | "place"::c::row::col::t 
      -> Place ((Board.make_tile (String.get c 0) ), 
                (Board.make_pos (int_of_string row) (int_of_string col)))
    | "remove"::row::col::t -> Remove (Board.make_pos (int_of_string row) (int_of_string col))
    | "inventory"::t -> Inventory
    | "endturn"::t -> Endturn
    | "score"::t -> Score
    | "help"::t -> Help
    | "board"::t -> Board
    | _ -> raise Malformed