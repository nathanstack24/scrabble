open Board

type command = 
  | Quit
  | Place of tile*position
  | Remove of Board.position
  | Endturn
  | Score
  | Help
  | Skip
  | Perfect
  (* | Swap of string *)


exception Empty
exception Malformed
exception BadSwapNum

let rec clean_str (lst:string list) : (string list) =
  match lst with
  | [] -> []
  | h::t -> if String.equal h "" then clean_str t
    else h::clean_str t

let parse (str:string) (st:State.t) : command =
  let cmd = (clean_str (String.split_on_char ' ' (String.trim str))) in
  if List.length cmd = 0 then raise Empty else 
    match cmd with 
    | "quit"::t -> Quit
    | "remove"::[] -> 
      let x = State.get_cursor_xpos st in 
      let y = State.get_cursor_ypos st in 
      Remove (Board.make_pos x y)
    | "endturn"::[] -> Endturn
    | "score"::[] -> Score
    | "help"::[] -> Help
    | "perfect"::[] -> Perfect
    | "skip"::[] -> Skip
    (* | "swap"::n::[] -> Swap n *)
    | c::[] -> 
      let x = State.get_cursor_xpos st in 
      let y = State.get_cursor_ypos st in 
      Place ((Board.make_tile (String.get c 0) ),(Board.make_pos x y))
    | _ -> raise Malformed