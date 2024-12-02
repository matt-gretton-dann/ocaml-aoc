(** [is_monotonic pred lst] returns true if [pred] returns true when tested on
    all consecutive elements of [lst]. *)
let rec is_monotonic pred = function
  | [] | _ :: [] -> true
  | h :: h' :: t -> pred h h' && is_monotonic pred (h' :: t)

let is_safe lst =
  (is_monotonic ( < ) lst || is_monotonic ( > ) lst)
  && is_monotonic (fun a b -> Aoc.distance1 a b <= 3) lst

let is_safe_dampened lst =
  let rec impl acc = function
    | [] -> is_safe acc
    | h :: t -> is_safe (acc @ t) || impl (acc @ [ h ]) t
  in
  impl [] lst

let nums_from_file fname =
  Aoc.strings_from_file fname |> List.map Aoc.nums_from_string

let day2402a lsts = List.filter is_safe lsts |> List.length
let day2402b lsts = List.filter is_safe_dampened lsts |> List.length

let _ =
  Aoc.main nums_from_file
    [ (string_of_int, day2402a); (string_of_int, day2402b) ]
