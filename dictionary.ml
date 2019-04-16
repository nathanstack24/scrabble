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

(** [read_dir handle acc] returns the list of file names in the directory
  * associated with [handle] prepended onto accumulator list [acc].
  * Requires: [handle] is of type dir_handle. [acc] is a string list, 
  * where each element is a previously read file name in the directory 
  * associated with [handle] *)
let rec read_dir handle acc = 
  try (
    let next_file = Unix.readdir handle in
    let updated_files= next_file :: acc in
    read_dir handle updated_files 
  ) 
  with
    End_of_file -> Unix.closedir handle; acc

(** [extract_words line] returns a list of all words in string list [line]
  * according to the definition of a word provided in the A4 Description.
  * Requires: [line] is a string list corresponding to a line of a 
  * text file. *)
let extract_words (line:string list) : string list = 
  let r = Str.regexp "[A-Z]+" in
  try 
    let f = fun str -> ignore (Str.search_forward r str 0); 
      Str.matched_string str in
    let l = List.map f line in
    List.map String.lowercase_ascii l
  with 
  | Not_found -> []

(** [parse_text l] returns a string list list, where each element of the 
 * list is a list containing a single word from the Scrabble dictionary text file.
 * 
 * Requires: [l] is a string list where each element of [l] is a string
 * corresponding to a line in the Scrabble dictionary text file *)
let parse_text (l: string list) = 
  let words_list = List.map 
      (Str.split (Str.regexp "[ \n\r\x0c\t]+")) l in
  let words = List.map extract_words words_list in
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

(** [construct_idx txt_list words_list acc] returns an index where all of
  * the words in each list in [words_list] are mapped to the files in 
  * [txt_list] that contain each of those words.
  *
  * Requires: - [txt_list] is a string list where each element is the name
  *             of a text file in a directory 
  *           - [words_list] is a string list list where each element is a
  *             list of words in a particular text file.
  *
  * Note that the indices of [txt_list] and [word_list] must also be
  * properly aligned; the first element of [sum_words] is a list 
  * containing all of the words in the the text file corresponding to the 
  * first element of [txt_list], and the second element of [sum_words] is 
  * a list containing all of the words in the the text file corresponding 
  * to the second element of [txt_list], etc. 
  * Thus, [List.length txt_list = List.length words_list]. Fails otherwise 
*)
let rec construct_idx txt_list words_list acc = 
  match (txt_list, words_list) with 
  | [],[] -> acc
  | (h1::t1), (h2::t2) -> let dict = add_words_to_dict h2 h1 acc in
    construct_idx t1 t2 dict
  | _ -> failwith "Error"

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


