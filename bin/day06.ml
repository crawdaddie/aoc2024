open Util
open Printf

let is_in_bounds (i, j) (m, n) = i < m && i >= 0 && j < n && j >= 0
let turn_right (di, dj) = (dj, -di)
let next (i, j) (di, dj) = (i + di, j + dj)

let part1 matrix =
  let dims = matrix_dims matrix in
  let pos = ch_matrix_find_exn '^' matrix in

  let mget (i, j) = matrix.(i).(j) in

  let rec traverse pos dir count =
    let count =
      match mget pos with
      | '.' ->
          let i, j = pos in
          matrix.(i).(j) <- 'x';
          count + 1
      | _ -> count
    in
    let next_pos = next pos dir in
    if not (is_in_bounds next_pos dims) then count
    else
      let next_char = mget next_pos in
      if next_char = '#' then
        let dir = turn_right dir in
        traverse (next pos dir) dir count
      else traverse (next pos dir) dir count
  in

  traverse pos (-1, 0) 1

let rec completes_loop coords pos dir =
  match coords with
  | [] -> false
  | (coord, prev_dir) :: rest ->
      let i, j = pos in
      let ci, cj = coord in
      if
        match dir with
        (* up *)
        | -1, 0 -> j = cj && ci < i && dir = prev_dir
        (* down *)
        | 1, 0 -> j = cj && ci > i && dir = prev_dir
        (* right *)
        | 0, 1 -> i = ci && cj > j && dir = prev_dir
        (* left *)
        | 0, -1 -> i = ci && cj < j && dir = prev_dir
        | _ -> failwith ""
      then true
      else completes_loop rest pos dir

let _print_char_matrix matrix =
  let m, n = matrix_dims matrix in
  for i = 0 to m - 1 do
    for j = 0 to n - 1 do
      Printf.printf "%c" matrix.(i).(j)
    done;
    Printf.printf "\n"
  done

let part2 matrix =
  let dims = matrix_dims matrix in
  let pos = ch_matrix_find_exn '^' matrix in
  let mget (i, j) = matrix.(i).(j) in

  let rec traverse pos dir count l =
    let count =
      if completes_loop l pos (turn_right dir) then count + 1 else count
    in
    let next_pos = next pos dir in
    if not (is_in_bounds next_pos dims) then count
    else
      let next_char = mget next_pos in
      match next_char with
      | '#' ->
          let ndir = turn_right dir in
          traverse (next pos ndir) ndir count ((pos, dir) :: l)
      | _ -> traverse (next pos dir) dir count l
  in

  let res = traverse pos (-1, 0) 0 [] in
  _print_char_matrix matrix;
  res

let () =
  (* let content = input_string "bin/inputs/day06_sml.txt" in *)
  let content = input_string "bin/inputs/day06.txt" in
  let matrix = char_matrix content in
  matrix |> part1 |> printf "part1: %d\n";
  matrix |> part2 |> printf "part2: %d\n"
