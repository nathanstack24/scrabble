(** Represents one of the possible commands for the game.*)
type command

(** The exception raised when a command is malformed*)
exception Malformed

(** The exception raised when a command is empty*)
exception Empty

(** Takes in a command string, cleans it to be interpreted. *)
val clean_str : string list -> string list

(**Converts the user input string to a command.*)
val parse : string -> command