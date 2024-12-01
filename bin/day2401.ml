(** [nums_from_string s] takes a string of space separated integers and gives
    back a list of the integers. *)
let nums_from_string s =
  List.map int_of_string (Str.split (Str.regexp " +") s);;

(** [pair_nums_from_string s] takes a string of two numbers separated by 
    whitespace and returns the pair of the numbers *)
let pair_nums_from_string s =
  match (nums_from_string s) with
  | h :: h' :: [] -> (h, h')
  | _ -> raise (Invalid_argument "pair_nums_from_string")

(** [unzip lst] takes a list of pairs and returns a pair of lists. *)
let unzip lst = 
  let rec impl acc acc' = function
    | (h, h') :: t -> impl (h :: acc) (h' :: acc') t
    | _ -> (acc, acc')
  in
  impl [] [] lst

(** [pairs_from_channel ch] returns the list of pairs given on the channel
    *)
let pairs_from_channel ch =
  let rec impl acc =
    try (impl ((input_line ch) :: acc)) with
    | End_of_file -> acc
  in
  impl [] |> List.map pair_nums_from_string

(** [pairs_from_file fname] returns the list of pairs given in the file *)
let pairs_from_file fname =
  try
    let ch = open_in fname in
    pairs_from_channel ch
  with
  | _ -> failwith "pairs_from_file"

(** [distance a b] returns the absolute difference between [a] and [b]. *)
let distance a b =
  abs (a - b)

let day2401a fname =
  let (a, b) = unzip (pairs_from_file fname) in
  let d = List.map2 distance (List.sort Int.compare a) (List.sort Int.compare b) in
  List.fold_left ( + ) 0 d

(** [count lst n] counts the number of times [n] appears as an element in [lst].
    *)
let count lst n =
  List.fold_left (fun acc x -> if x = n then (acc + 1) else acc) 0 lst
  
let day2401b fname =
  let (a, b) = unzip (pairs_from_file fname) in
  List.map (count b) a |>
  List.map2 ( * ) a |>
  List.fold_left ( + ) 0

let _ = try
  begin
    match Sys.argv with
    | [|_; fname|] ->
      Printf.printf "Part 1 = %d\n" (day2401a fname);
      Printf.printf "Part 2 = %d\n" (day2401b fname);
    | _ ->
      Printf.printf "Usage: day2401 <fname>\n";
      exit 1
  end
with
| e ->
  Printf.printf "An error occured: %s\n" (Printexc.to_string e);
  exit 1