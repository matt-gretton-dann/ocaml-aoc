let nums_from_string s = List.map int_of_string (Str.split (Str.regexp " +") s)
let distance1 a b = abs (a - b)

let strings_from_file fname =
  In_channel.with_open_text fname In_channel.input_lines
