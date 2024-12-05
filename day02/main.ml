let read_lines path = In_channel.with_open_bin path In_channel.input_all
let lines = String.split_on_char '\n'
let levels s = List.map int_of_string @@ String.split_on_char ' ' s

let safe_levels a b c =
  abs (a - b) < 4 && abs (b - c) < 4 && ((a > b && b > c) || (a < b && b < c))

let rec is_safe = function
  | [] | [ _ ] -> true
  | [ a; b ] -> if abs (a - b) < 4 then true else false
  | a :: (b :: c :: _ as t) -> if safe_levels a b c then is_safe t else false

let part_1 lst =
  List.fold_left (fun acc a -> if is_safe a then acc + 1 else acc) 0 lst

let part_2 lst =
  let try_all lst =
    let rec aux hd = function
      | [] -> false
      | x :: xs -> is_safe (hd @ xs) || aux (hd @ [ x ]) xs
    in
    aux [] lst
  in
  List.fold_left (fun acc a -> if try_all a then acc + 1 else acc) 0 lst

let () =
  let reports =
    read_lines "input.txt"
    |> lines
    |> List.filter (( <> ) "")
    |> List.map levels
  in
  Printf.printf "Part 1: %d\n" (part_1 reports);
  Printf.printf "Part 2: %d" (part_2 reports)
