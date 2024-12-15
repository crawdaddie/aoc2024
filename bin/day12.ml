open Util
open Printf

let compute_sq_perimeter (i, j) matrix =
  let char = matrix.(i).(j) in
  let neighbors = [ (i + 1, j); (i - 1, j); (i, j + 1); (i, j - 1) ] in
  let dims = matrix_dims matrix in

  let perim =
    List.fold_left
      (fun acc (i, j) ->
        if not (in_bounds dims (i, j)) then acc + 1
        else if matrix.(i).(j) <> char then acc + 1
        else acc)
      0 neighbors
  in

  perim

module IntPairs = struct
  type t = int * int

  let compare (x0, y0) (x1, y1) =
    match Stdlib.compare x0 x1 with 0 -> Stdlib.compare y0 y1 | c -> c
end

module PairsSet = Set.Make (IntPairs)

let get_region matrix state pos perim_func =
  let i, j = pos in
  let ch = matrix.(i).(j) in
  let m = PairsSet.empty in
  let dims = matrix_dims matrix in

  let rec search (i, j) set perim =
    if in_bounds dims (i, j) && matrix.(i).(j) = ch && state.(i).(j) <> '.' then (
      state.(i).(j) <- '.';
      let s = PairsSet.add (i, j) set in
      let perim = perim + perim_func (i, j) matrix in

      [ (i + 1, j); (i - 1, j); (i, j + 1); (i, j - 1) ]
      |> List.fold_left (fun (s, perim) p -> search p s perim) (s, perim))
    else (set, perim)
  in
  let set, perim = search pos m 0 in
  let area = List.length (PairsSet.elements set) in
  let res = area * perim in
  res

let part1 matrix =
  let m, n = matrix_dims matrix in
  let state = Array.init m (fun i -> Array.init n (fun j -> '0')) in
  let sum = ref 0 in
  for i = 0 to m - 1 do
    for j = 0 to n - 1 do
      if state.(i).(j) <> '.' then
        sum := !sum + get_region matrix state (i, j) compute_sq_perimeter
    done
  done;
  !sum

(**

...
.a. -> diffs [1,1,1,1,1,1,1,1] -> 4 corners
...

.aa
.aa -> diffs [1,0,0,0,0,0,1,1] -> 4 corners
.aa

*)

let count_corners (i, j) matrix =
  let char = matrix.(i).(j) in
  let neighbors =
    [
      (i - 1, j - 1);
      (i - 1, j);
      (i - 1, j + 1);
      (i, j + 1);
      (i + 1, j + 1);
      (i + 1, j);
      (i + 1, j - 1);
      (i, j - 1);
    ]
  in

  let dims = matrix_dims matrix in
  let diff_count =
    List.filter
      (fun (i, j) -> (not (in_bounds dims (i, j))) || matrix.(i).(j) <> char)
      neighbors
  in
  (* match List.length diff_count with 0 -> 0 | 8 -> 4 | 7 -> 2 | 6 -> 2 | 5 -> 2 | 4 -> 1 | 3 -> 1 |  *)
  0

(* Count corner configurations at a position *)
let count_corners (i, j) matrix =
  let dims = matrix_dims matrix in
  let target_char = matrix.(i).(j) in
  let neighbors =
    [|
      (i - 1, j - 1);
      (i - 1, j);
      (i - 1, j + 1);
      (i, j + 1);
      (i + 1, j + 1);
      (i + 1, j);
      (i + 1, j - 1);
      (i, j - 1);
    |]
  in

  let diff_neighbors =
    Array.map
      (fun (i, j) ->
        if not (in_bounds dims (i, j)) then 1
        else if matrix.(i).(j) <> target_char then 1
        else 0)
      neighbors
  in

  match diff_neighbors with
  | [| 1; 1; 1; 1; 1; 1; 1; 1; 1 |] -> 4
  | [| 0; 0; 0; 0; 0; 0; 0; 0; 0 |] -> 0
  | _ -> 0

let part2 matrix =
  let m, n = matrix_dims matrix in
  let state = Array.init m (fun i -> Array.init n (fun j -> '0')) in
  let sum = ref 0 in
  for i = 0 to m - 1 do
    for j = 0 to n - 1 do
      if state.(i).(j) <> '.' then
        sum := !sum + get_region matrix state (i, j) count_corners
    done
  done;
  !sum

let () =
  let content = input_string "bin/inputs/day12_sml.txt" in
  content |> char_matrix |> part1 |> printf "part1: %d\n";
  content |> char_matrix |> part2 |> printf "part2: %d\n"
