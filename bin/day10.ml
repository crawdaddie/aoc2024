open Util
open Printf

let calculate_trail_score = 1

let part1 map =
  let get_neighbors pos =
    let v = map.(fst pos).(snd pos) in
    [ vadd pos (-1, 0); vadd pos (1, 0); vadd pos (0, -1); vadd pos (0, 1) ]
    |> List.filter (in_bounds (matrix_dims map))
    |> List.filter (fun (i, j) -> map.(i).(j) - v = 1)
  in

  let hget (i, j) = map.(i).(j) in

  let set () =
    let m, n = matrix_dims map in
    Array.init m (fun i -> Array.init n (fun j -> 0))
  in

  let _get m (i, j) = m.(i).(j) in

  let rec compute_path start set =
    match hget start with
    | 9 when _get set start = 1 -> 0
    | 9 ->
        let i, j = start in
        set.(i).(j) <- 1;
        1
    | _ ->
        let neighbors = get_neighbors start in
        List.fold_left (fun acc st -> acc + compute_path st set) 0 neighbors
  in

  let zeros = matrix_find_all (( = ) 0) map in
  List.fold_left (fun acc st -> acc + compute_path st (set ())) 0 zeros

let part2 map =
  let get_neighbors pos =
    let v = map.(fst pos).(snd pos) in
    [ vadd pos (-1, 0); vadd pos (1, 0); vadd pos (0, -1); vadd pos (0, 1) ]
    |> List.filter (in_bounds (matrix_dims map))
    |> List.filter (fun (i, j) -> map.(i).(j) - v = 1)
  in

  let hget (i, j) = map.(i).(j) in

  let rec compute_path start =
    match hget start with
    | 9 -> 1
    | _ ->
        let neighbors = get_neighbors start in
        List.fold_left (fun acc st -> acc + compute_path st) 0 neighbors
  in

  let zeros = matrix_find_all (( = ) 0) map in
  List.fold_left (fun acc st -> acc + compute_path st) 0 zeros

let () =
  let content = input_string "bin/inputs/day10.txt" in
  let map = content |> char_matrix |> Array.map (Array.map char_to_int) in
  map |> part1 |> printf "part1: %d\n";
  map |> part2 |> printf "part2: %d\n"
