val nums_from_string : string -> int list
(** [nums_from_string s] takes a string of space separated integers and gives
    back a list of the integers. *)

val distance1 : int -> int -> int
(** [distance1 a b] returns the absolute difference between [a] and [b]. *)

val strings_from_file : string -> string list
(** [strings_from_file fname] returns a list of strings from the file 
    [fname].  Each string represents a line from the file. *)

val main : (string -> 'a) -> (('b -> string) * ('a -> 'b)) list -> unit
(** [main prep parts] executes an advent of code problem.  [prep fname] should
    be a function that returns the input from [fname].  Each elemet of
    [parts] is a pair of functions.  The first converts the output to a string
    (for example [string_of_int]).  The second executes the given part.
    Output is given as if done by: 
    [print_string ( prep fname |> snd |> fst )] *)
