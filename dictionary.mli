(** 
   Representation of a Scrabble dictionary as a set.
*)

(**********************************************************************)

(** [Dict] is the module used to maintain a Scrabble dictionary *)
module Dict: Set.S with type elt = string


(** [create_dictionary] is a dictionary containing all words in the official
  * Scrabble dictionary*)
val create_dictionary: Dict.t

(** [is_member s dict] tests whether or not input string [s] is a member of 
  * dictionary [dict] *)
val is_member: Dict.t -> string -> bool



