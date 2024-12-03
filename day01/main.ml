let read_lines (path : string) : string =
  In_channel.with_open_bin path In_channel.input_all

let lines = String.split_on_char '\n'
let () = read_lines "input.txt" |> lines |> List.hd |> print_endline
