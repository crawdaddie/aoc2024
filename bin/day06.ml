open Util
open Printf

let is_in_bounds (i, j) (m, n) = i < m && i >= 0 && j < n && j >= 0
let turn_right (di, dj) = (dj, -di)
let next (i, j) (di, dj) = (i + di, j + dj)

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

let part1 matrix path_state =
  let pos = ch_matrix_find_exn '^' matrix in

  let count = ref 0 in
  let _ =
    traverse
      (fun (i, j) dir ->
        let () =
          match path_state.(i).(j) with
          | 0, 0 ->
              path_state.(i).(j) <- dir;
              count := !count + 1
          | _ -> ()
        in
        KeepSearching)
      pos (-1, 0) matrix
  in
  !count

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

let _print_char_matrix matrix =
  let m, n = matrix_dims matrix in
  for i = 0 to m - 1 do
    for j = 0 to n - 1 do
      Printf.printf "%c" matrix.(i).(j)
    done;
    Printf.printf "\n"
  done

let part2 matrix path_state =
  let m, n = matrix_dims matrix in
  let pos = ch_matrix_find_exn '^' matrix in
  let get_in_m (i, j) = matrix.(i).(j) in

  let count = ref 0 in

  let _ =
    traverse
      (fun (i, j) dir ->
        let next_pos = next (i, j) dir in
        let next_char () = get_in_m (next (i, j) dir) in
        if is_in_bounds next_pos (m, n) && next_char () <> '#' then
          let loop_state =
            Array.init m (fun i -> Array.init n (fun j -> path_state.(i).(j)))
          in
          let () =
            match
              traverse
                (fun (i, j) dir ->
                  match loop_state.(i).(j) with
                  | prevdir when prevdir = dir -> Found
                  | 0, 0 ->
                      loop_state.(i).(j) <- dir;
                      KeepSearching
                  | _ -> KeepSearching)
                pos (turn_right dir) matrix
            with
            | Found ->
                printf "found loop at %d %d\n" i j;
                let ni, nj = next_pos in
                if matrix.(ni).(nj) <> 'L' then (
                  matrix.(ni).(nj) <- 'L';
                  count := !count + 1)
            | _ -> ()
          in
          KeepSearching
        else KeepSearching)
      pos (-1, 0) matrix
  in
  !count

let () =
  let content = input_string "bin/inputs/day06_sml.txt" in
  (* let content = input_string "bin/inputs/day06.txt" in *)
  let matrix = char_matrix content in
  let m, n = matrix_dims matrix in
  let path_state = Array.init m (fun i -> Array.init n (fun j -> (0, 0))) in

  part1 matrix path_state |> printf "part1: %d\n";
  part2 matrix path_state |> printf "part2: %d\n"

(* char_matrix content |> part2 |> printf "part2: %d\n" *)
