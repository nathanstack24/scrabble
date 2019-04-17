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
  | t -> raise Not_found


(** [point_values] is a list of tuples containing all letters in the alphabet
  * and the number of points associated with that letter *)
let point_values = [('A',1); ('E',1); ('I',1); ('O',1); ('U',1); ('L',1); ('N',1); 
                    ('S',1); ('T',1); ('R',1); ('D',2); ('G',2); ('B',3); ('C',3); 
                    ('M',3); ('P',3); ('F',4); ('H',4); ('V',4); ('W',4); ('Y',4);
                    ('K',5); ('J',8); ('X',8); ('Q',10); ('Z',10)]


let tile_values = 
  let rec loop (map: int Values.t) (lst: (char*int) list ) = 
    match lst with 
    | [] -> map
    | h::t -> loop (Values.add (fst h) (snd h) map) t in 
  loop (Values.empty) point_values

let rec add_tile_num_times (c:char) (num: int) (acc: char list) = 
  if num=0 then acc else
    add_tile_num_times c (num-1) (c::acc)

let init_tile_bag = 
  (* one point tiles *)
  let acc = add_tile_num_times 'E' 12 [] in 
  let acc = (add_tile_num_times 'A' 9 []) @ acc in 
  let acc = (add_tile_num_times 'I' 9 []) @ acc in 
  let acc = (add_tile_num_times 'O' 8 []) @ acc in 
  let acc = (add_tile_num_times 'N' 6 []) @ acc in 
  let acc = (add_tile_num_times 'R' 6 []) @ acc in
  let acc = (add_tile_num_times 'T' 6 []) @ acc in
  let acc = (add_tile_num_times 'L' 4 []) @ acc in   
  let acc = (add_tile_num_times 'S' 4 []) @ acc in 
  let acc = (add_tile_num_times 'U' 4 []) @ acc in 

  (* two point tiles *)
  let acc = (add_tile_num_times 'D' 4 []) @ acc in 
  let acc = (add_tile_num_times 'G' 3 []) @ acc in 

  (* three point tiles *)
  let acc = (add_tile_num_times 'B' 2 []) @ acc in 
  let acc = (add_tile_num_times 'C' 2 []) @ acc in 
  let acc = (add_tile_num_times 'M' 2 []) @ acc in 
  let acc = (add_tile_num_times 'P' 2 []) @ acc in 

  (* four point tiles *)
  let acc = (add_tile_num_times 'F' 2 []) @ acc in 
  let acc = (add_tile_num_times 'H' 2 []) @ acc in 
  let acc = (add_tile_num_times 'V' 2 []) @ acc in
  let acc = (add_tile_num_times 'W' 2 []) @ acc in
  let acc = (add_tile_num_times 'Y' 2 []) @ acc in

  (* five point tile *)
  let acc = (add_tile_num_times 'K' 1 []) @ acc in

  (* eight point tiles *)
  let acc = (add_tile_num_times 'J' 1 []) @ acc in
  let acc = (add_tile_num_times 'X' 1 []) @ acc in

  (* ten point tiles *)
  let acc = (add_tile_num_times 'Q' 1 []) @ acc in
  (add_tile_num_times 'Z' 1 []) @ acc






