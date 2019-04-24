(** 
   Representation of a Scrabble dictionary as a set.
*)

(**********************************************************************)

(** [Values] is the module used to determine the number of points that each
  * letter is worth in Scrabble *)
module Values: Map.S with type key = char

(** [dict_hash_table] is a hash table containing all words in the official
  * Scrabble dictionary. The keys in the hashtable are strings which map to
  * unit values. *)
val dict: (string,unit) Hashtbl.t

(** [tile_values] is a mapping from all characters in the alphabet to the number
  * of points that particular character is worth in Scrabble. *)
val tile_values: int Values.t

(** [init_tile_bag] is the initial tile bag used in a game of scrabble *)
val init_tile_bag: char list



