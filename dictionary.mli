(** 
   Representation of a Scrabble dictionary as a set.
*)

(**********************************************************************)

(** [Dict] is the module used to maintain a Scrabble dictionary *)
module Dict: Set.S

(** [create_dictionary] is a dictionary containing all words in the official
  * Scrabble dictionary*)
val create_dictionary: Dict.t



