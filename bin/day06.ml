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

let print_tup (x, y) =
  printf "(%d %d)" x y;
  ()

let rec completes_loop pos dir state matrix =
  let dims = matrix_dims matrix in
  let rec traverse pos dir =
    let i, j = pos in
    let prevdir = state.(i).(j) in
    if prevdir = dir then true
    else (
      state.(i).(j) <- dir;

      let next_pos = next pos dir in
      if not (is_in_bounds next_pos dims) then false
      else
        let ni, nj = next_pos in
        let next_char = matrix.(ni).(nj) in
        match next_char with
        | '#' ->
            let ndir = turn_right dir in
            traverse (next pos ndir) ndir
        | _ -> traverse (next pos dir) dir)
  in
  traverse pos dir

type traverse_result = Found | Stop | KeepSearching

let rec traverse f pos dir matrix =
  let dims = matrix_dims matrix in
  let rec traverse pos dir =
    match f pos dir with
    | Found -> Found
    | Stop -> Stop
    | KeepSearching -> (
        let next_pos = next pos dir in
        if not (is_in_bounds next_pos dims) then Stop
        else
          let ni, nj = next_pos in
          let next_char = matrix.(ni).(nj) in
          match next_char with
          | '#' ->
              let ndir = turn_right dir in
              traverse (next pos ndir) ndir
          | _ -> traverse (next pos dir) dir)
  in
  traverse pos dir

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
  let m, n = dims in
  (* let state = Array.init m (fun i -> Array.init n (fun j -> (0, 0))) in *)
  let pos = ch_matrix_find_exn '^' matrix in
  let count = ref 0 in

  let get_tuple_matrix () =
    Array.init m (fun i -> Array.init n (fun j -> (0, 0)))
  in

  let find_loop_state = get_tuple_matrix () in
  let _res =
    traverse
      (fun pos dir ->
        let i, j = pos in
        let () =
          let marker = get_tuple_matrix () in
          (* find loop by pretending there's an obstacle ahead and turning right *)
          match
            traverse
              (fun pos dir ->
                let i, j = pos in
                let prevdir = find_loop_state.(i).(j) in
                match prevdir with
                | prevdir when prevdir = dir -> Found
                | _ ->
                    if marker.(i).(j) = dir then Stop
                    else (
                      marker.(i).(j) <- dir;
                      KeepSearching))
              pos (turn_right dir) matrix
          with
          | Found -> count := !count + 1
          | _ -> find_loop_state.(i).(j) <- dir
        in
        KeepSearching)
      pos (-1, 0) matrix
  in
  !count

let () =
  (* let content = input_string "bin/inputs/day06_sml.txt" in *)
  let content = input_string "bin/inputs/day06.txt" in
  (* let matrix = char_matrix content in *)
  char_matrix content |> part1 |> printf "part1: %d\n";
  char_matrix content |> part2 |> printf "part2: %d\n"
