module Dict = Set.Make(String)

module Values = Map.Make(Char)


(** [read_channel channel acc] returns a list of all lines in the text file
  * corresponding to the input channel [channel] prepended onto string 
  * list [acc]. Each element of [acc] corresponds to a line in the text 
  * file that was previously read.
  * Requires: [channel] is an in_channel, [acc] is a string list.   *)
let rec read_channel channel acc =
  try (
    let next_line = Pervasives.input_line channel in
    let updated = next_line::acc in
    read_channel channel updated )
  with  
    End_of_file -> Pervasives.close_in channel; acc

(** [parse_text l] returns a string list, where each element of the 
 * list is a single word from the Scrabble dictionary text file.
 * 
 * Requires: [l] is a string list where each element of [l] is a string
 * corresponding to a line in the Scrabble dictionary text file *)
let parse_text (l: string list) = 
  let words_list = List.map 
      (Str.split (Str.regexp "[ \n\r\x0c\t]+")) l in
  let words = List.flatten words_list in
  words

(** [add_words_to_dict list txt acc] returns a set with every word in   
  * list [word] added to the set [acc] (which is acting as our Scrabble dictionary). 
  * Requires: - [list] is a string list where each element is a word in
  *             text file "scrabble_dict.txt".
  *           - [acc] is a   *)
let rec add_words_to_dict (words: string list) (acc:Dict.t) = 
  match words with 
  | [] -> acc
  | h::t -> add_words_to_dict t (Dict.add h acc)


let create_dictionary =
  try 
    let dict_text_file = Unix.getcwd() ^ Filename.dir_sep ^ "scrabble_dict.txt" in
    let channel = Pervasives.open_in dict_text_file in
    let all_words_list = read_channel channel [] in       
    let all_words = parse_text all_words_list in 
    add_words_to_dict all_words Dict.empty
  with 
  | t -> raise Not_found(* Throwing error *)


let points = [('A',1); ('E',1); ('I',1); ('O',1); ('U',1); ('L',1); ('N',1); 
              ('S',1); ('T',1); ('R',1)]


let tile_values = 


  (* TODO : revisit this *)
  let is_member (dict: Dict.t) (str: string) = Dict.mem str dict



