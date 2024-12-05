let read_lines path = In_channel.with_open_bin path In_channel.input_all
let lines = String.split_on_char '\n'

let pairs str =
  let l = String.split_on_char ' ' str |> List.filter_map int_of_string_opt in
  (List.hd l, List.nth l 1)

let part_1 a b =
  let sorted_left = List.sort ( - ) a in
  let sorted_right = List.sort ( - ) b in
  List.fold_left2 (fun acc a b -> acc + abs (a - b)) 0 sorted_left sorted_right

let () =
  let a, b =
    read_lines "input.txt"
    |> lines
    |> List.filter (( <> ) "")
    |> List.map pairs
    |> List.split
  in
  Printf.printf "Part 1: %d\n" (part_1 a b)
