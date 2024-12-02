(** [pair_nums_from_string s] takes a string of two numbers separated by 
    whitespace and returns the pair of the numbers *)
let pair_nums_from_string s =
  match Aoc.nums_from_string s with
  | [ h; h' ] -> (h, h')
  | _ -> raise (Invalid_argument "pair_nums_from_string")

(** [rev_split lst] takes a list of pairs and returns a pair of lists.  Is 
    equivalent to List.split (List.rev lst) but more efficient (and tail 
    recursive). *)
let rev_split lst =
  let rec impl acc acc' = function
    | (h, h') :: t -> impl (h :: acc) (h' :: acc') t
    | _ -> (acc, acc')
  in
  impl [] [] lst

(** [count lst n] counts the number of times [n] appears as an element in [lst].
    *)
let count lst n =
  List.fold_left (fun acc x -> if x = n then acc + 1 else acc) 0 lst

(** [accumulate lst] sums all the elements of [lst]. *)
let accumulate = List.fold_left ( + ) 0

(** [lists_from_file fname] Read two lists of integers from [fname] and return
    as a pair. *)
let lists_from_file fname =
  Aoc.strings_from_file fname |> List.map pair_nums_from_string |> rev_split

let day2401a a b =
  List.map2 Aoc.distance1 (List.sort Int.compare a) (List.sort Int.compare b)
  |> accumulate

let day2401b a b = List.map (count b) a |> List.map2 ( * ) a |> accumulate

let _ =
  try
    match Sys.argv with
    | [| _; fname |] ->
        let a, b = lists_from_file fname in
        Printf.printf "Part 1 = %d\n" (day2401a a b);
        Printf.printf "Part 2 = %d\n" (day2401b a b)
    | _ ->
        Printf.printf "Usage: day2401 <fname>\n";
        exit 1
  with e ->
    Printf.printf "An error occured: %s\n" (Printexc.to_string e);
    exit 1
