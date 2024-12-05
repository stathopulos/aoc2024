let read_lines path = In_channel.with_open_bin path In_channel.input_all
let lines = String.split_on_char '\n'

let pairs str =
  let l = String.split_on_char ' ' str |> List.filter_map int_of_string_opt in
  (List.hd l, List.nth l 1)

let part_1 a b =
  let sorted_left = List.sort ( - ) a in
  let sorted_right = List.sort ( - ) b in
  List.fold_left2 (fun acc a b -> acc + abs (a - b)) 0 sorted_left sorted_right

let part_2 left right =
  let count x lst = List.length @@ List.filter (( = ) x) lst in
  List.fold_left (fun acc x -> acc + (x * count x right)) 0 left

let () =
  let a, b =
    read_lines "input.txt"
    |> lines
    |> List.filter (( <> ) "")
    |> List.map pairs
    |> List.split
  in
  Printf.printf "Part 1: %d\n" (part_1 a b);
  Printf.printf "Part 2: %d" (part_2 a b)
