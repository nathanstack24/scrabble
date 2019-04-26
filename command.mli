(** Represents one of the possible commands for the game.*)
type command = 
  | Quit
  | Place of Board.tile*Board.position
  | Remove of Board.position
  | Endturn
  | Score
  | Help
  | Skip
  | Perfect 
  (*| Swap of string*)

(** The exception raised when a command is malformed*)
exception Malformed

(** The exception raised when the user tries to *)
exception BadSwapNum

(** The exception raised when a command is empty*)
exception Empty

(** [clean_str strs] removes any whitespace in all strings in
    string list [strs]. *)
val clean_str : string list -> string list

(** [parse state str] performs the appropriate command based on the user-inputed
    string [str]. If the user inputs nothing, no command is executed.  *)
val parse : string -> State.t -> command