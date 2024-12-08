open Util
open Printf

let idx_to_char idx =
  match idx with
  | idx when idx >= 0 && idx <= 25 -> Char.chr (idx + Char.code 'a')
  | idx when idx >= 26 && idx <= 51 -> Char.chr (idx - 26 + Char.code 'A')
  | idx when idx >= 52 && idx <= 61 -> Char.chr (idx - 52 + Char.code '0')
  | _ -> failwith "idx->char out of range"

let rec char_to_idx ch =
  match ch with
  | 'a' .. 'z' -> Char.code ch - Char.code 'a'
  | 'A' .. 'Z' -> Char.code ch - Char.code 'A' + char_to_idx 'z' + 1
  | '0' .. '9' -> Char.code ch - Char.code '0' + char_to_idx 'Z' + 1
  | _ -> failwith "char->idx out of range"

module CharMap = struct
  type t = int * int list array

  let create () = Array.init 62 (fun _ -> [])

  let add_at map key value =
    let idx = char_to_idx key in
    map.(idx) <- value :: map.(idx)

  let get_pairs map key =
    let idx = char_to_idx key in
    ordered_pairs map.(idx)

  let get_pairs_at_idx map idx = ordered_pairs map.(idx)

  let pairs_fold_left f acc map =
    let r = ref acc in
    for i = 0 to 61 do
      if map.(i) <> [] then
        let pairs = get_pairs_at_idx map i in
        let char = idx_to_char i in
        r := f !r pairs char
    done;
    !r
end

let char_map_from_matrix matrix =
  let map = CharMap.create () in
  let m, n = matrix_dims matrix in
  for i = 0 to m - 1 do
    for j = 0 to n - 1 do
      match matrix.(i).(j) with '.' -> () | x -> CharMap.add_at map x (i, j)
    done
  done;
  map

let part1 matrix =
  (* print_char_matrix matrix; *)
  let m, n = matrix_dims matrix in
  let vadd (i, j) (k, l) = (i + k, j + l) in
  let vmul (i, j) a = (i * a, j * a) in
  let copy = Array.init m (fun i -> Array.init n (fun j -> '.')) in

  let compute_antinodes (i, j) (k, l) =
    let vec = (k - i, l - j) in
    let search pos acc =
      if not (is_in_bounds pos (m, n)) then acc
      else
        let i, j = pos in
        match copy.(i).(j) with
        | '.' ->
            copy.(i).(j) <- '#';
            acc + 1
        | '#' -> acc
        | _ -> failwith ""
    in

    search (vadd (i, j) (vmul vec 2)) 0
  in

  let map = char_map_from_matrix matrix in

  CharMap.pairs_fold_left
    (fun acc pairs _ch ->
      acc
      + List.fold_left (fun acc (a, b) -> acc + compute_antinodes a b) 0 pairs)
    0 map

let part2 matrix =
  let m, n = matrix_dims matrix in
  let vadd (i, j) (k, l) = (i + k, j + l) in

  let state = Array.init m (fun i -> Array.init n (fun j -> '.')) in

  let compute_antinodes (i, j) (k, l) =
    let vec = (k - i, l - j) in
    let rec search pos acc =
      if not (is_in_bounds pos (m, n)) then acc
      else
        let i, j = pos in
        match state.(i).(j) with
        | '.' ->
            state.(i).(j) <- '#';
            search (vadd pos vec) (acc + 1)
        | '#' -> search (vadd pos vec) acc
        | _ -> failwith ""
    in

    search (vadd (i, j) vec) 0
  in

  let map = char_map_from_matrix matrix in
  CharMap.pairs_fold_left
    (fun acc pairs _ch ->
      acc
      + List.fold_left (fun acc (a, b) -> acc + compute_antinodes a b) 0 pairs)
    0 map

let () =
  let content = input_string "bin/inputs/day08.txt" in
  let matrix = char_matrix content in
  let m, n = matrix_dims matrix in

  matrix |> part1 |> printf "part1: %d\n";
  matrix |> part2 |> printf "part2: %d\n"
