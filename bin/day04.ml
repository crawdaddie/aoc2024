open Util
open Printf

let char_matrix content =
  let lines = content |> String.trim |> String.split_on_char '\n' in
  let matrix = Array.of_list (List.map array_of_string lines) in
  matrix

let is_in_bounds i j m n = i < m && i >= 0 && j < n && j >= 0

let get_string_from_matrix (i, j) (di, dj) num matrix =
  let m, n = matrix_dims matrix in
  let in_bounds = is_in_bounds (i + (3 * di)) (j + (3 * dj)) m n in
  if in_bounds then
    let str =
      String.init num (fun idx -> matrix.(i + (idx * di)).(j + (idx * dj)))
    in
    Some str
  else None

let find_xmas_in_direction pos dir matrix =
  let str = get_string_from_matrix pos dir 4 matrix in
  match str with Some "XMAS" -> 1 | _ -> 0

let dirs =
  [ (1, 1); (1, 0); (1, -1); (0, -1); (-1, -1); (-1, 0); (-1, 1); (0, 1) ]

let find_xmas_in_directions pos matrix =
  List.fold_left
    (fun acc dir -> acc + find_xmas_in_direction pos dir matrix)
    0 dirs

let part1 matrix =
  matrix_fold
    (fun pos matrix acc -> acc + find_xmas_in_directions pos matrix)
    matrix 0

let get_string_from_matrix_exn (i, j) (di, dj) num matrix =
  String.init num (fun idx -> matrix.(i + (idx * di)).(j + (idx * dj)))

let check_for_mas (i, j) matrix =
  let diag1 = get_string_from_matrix_exn (i - 1, j - 1) (1, 1) 3 matrix in
  let diag2 = get_string_from_matrix_exn (i - 1, j + 1) (1, -1) 3 matrix in
  match (diag1, diag2) with
  | "MAS", "SAM" | "SAM", "SAM" | "MAS", "MAS" | "SAM", "MAS" -> 1
  | _ -> 0

let part2 matrix =
  let m, n = matrix_dims matrix in
  ij_fold
    (fun (i, j) acc ->
      let ch = matrix.(i).(j) in
      if ch = 'A' then acc + check_for_mas (i, j) matrix else acc)
    (1, m - 2)
    (1, n - 2)
    0

let () =
  let content = input_string "bin/inputs/day04.txt" in
  let matrix = char_matrix content in
  matrix |> part1 |> printf "part1: %d\n";
  matrix |> part2 |> printf "part2: %d\n"
