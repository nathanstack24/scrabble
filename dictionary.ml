module String_type: Set.OrderedType = struct
  type t = string
  let compare x y =
    String.compare x y
end

module Dict:Set.S = Set.Make(String_type)

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

(** [add_words_to_dict list txt acc] returns an index with every word in   
  * string list [list] added to the index [acc] (with corresponding key
  * [txt], the text file which contains each word in [list]). 
  * Requires: - [list] is a string list where each element is a word in
  *             text file [txt].
  *           - [txt] is a string corresponding to the name of a text file
  *             (with extension .txt)
  *           - [acc] is an index  *)
let rec add_words_to_dict (list:string list) (txt:string) (acc:idx) = 
  match list with 
  | [] -> acc
  | h::t ->
    let set = Dict.find h acc in 
    match set with 
    | None -> add_words_to_dict t txt (Dict.insert h 
                                         (Set.insert txt Set.empty) acc)
    | Some s -> let new_set = Set.insert txt s in
      add_words_to_dict t txt (Dict.insert h new_set acc)

let create_dictionary (dict:string) : idx =
  try 
    let dict_text_file = Unix.getcwd() ^ Filename.dir_sep ^ "scrabble_dict.txt" in 
    let channel = Pervasives.open_in dict_text_file in
    let all_words_list = read_channel channel [] in             
    let sub_strings= parse_words all_words_list in
    let sum_words = List.map List.concat sub_strings in
    let uniq_sum_words = List.map (List.sort_uniq String.compare) 
        sum_words in
    construct_idx txt_list uniq_sum_words Dict.empty
  with 
  | t -> raise Not_found


