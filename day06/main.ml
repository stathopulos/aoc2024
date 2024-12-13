let read_lines path = In_channel.with_open_bin path In_channel.input_all
let lines = String.split_on_char '\n'

type direction = Up | Down | Left | Right

let turn = function
  | Up -> Right
  | Down -> Left
  | Left -> Up
  | Right -> Down

let into_map s = List.map String.to_bytes s |> Array.of_list

let find_position c =
  Array.find_mapi (fun i x ->
      Option.map (fun a -> (i, a)) (Bytes.index_opt x c))

let safe_map_get map (x, y) =
  try Some (Bytes.get (Array.get map x) y) with _ -> None

let map_set map (x, y) c = Bytes.set (Array.get map x) y c

let part_1 map =
  let start = Option.get @@ find_position '^' map in
  let rec aux (x, y) direction acc =
    let next =
      match direction with
      | Up -> (x - 1, y)
      | Down -> (x + 1, y)
      | Left -> (x, y - 1)
      | Right -> (x, y + 1)
    in
    match safe_map_get map next with
    | Some '#' -> aux (x, y) (turn direction) acc
    | Some '.' ->
        map_set map (x, y) 'X';
        aux next direction (acc + 1)
    | Some 'X' ->
        map_set map (x, y) 'X';
        aux next direction acc
    | None ->
        map_set map (x, y) 'X';
        acc + 1
    | Some ch -> failwith ("Unexpected input!" ^ String.make 1 ch)
  in
  aux start Up 0

let () =
  let map =
    read_lines "input.txt" |> lines |> List.filter (( <> ) "") |> into_map
  in
  Printf.printf "Part 1: %d" (part_1 map)
