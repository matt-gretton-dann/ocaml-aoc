let nums_from_string s = List.map int_of_string (Str.split (Str.regexp " +") s)
let distance1 a b = abs (a - b)

let strings_from_file fname =
  In_channel.with_open_text fname In_channel.input_lines

let main prep parts =
  try
    match Sys.argv with
    | [| _; fname |] ->
        let lines = prep fname in
        let do_part i (fmt, fn) =
          Printf.printf "Part %d = %s\n" i (fmt (fn lines))
        in
        List.iteri do_part parts;
        exit 0
    | _ ->
        Printf.printf "Usage: %s <fname>\n" Sys.executable_name;
        exit 2
  with e ->
    Printf.printf "An error occured: %s\n" (Printexc.to_string e);
    exit 1
