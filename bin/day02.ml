open Util
open Printf

let process_levels levels_seq =
  Seq.map
    (fun levels_str ->
      match String.split_on_char ' ' levels_str with
      | [] -> failwith "empty line"
      | l -> List.map (fun levels_str -> int_of_string levels_str) l)
    levels_seq

let get_diffs levels =
  let rec aux levels prev diffs =
    match levels with
    | [] -> diffs
    | hd :: rest ->
        let d = hd - prev in
        aux rest hd (d :: diffs)
  in
  List.rev (aux (List.tl levels) (List.hd levels) [])

let same_sign diffs =
  let rec aux diffs prev_sign =
    match diffs with
    | [] -> true
    | hd :: rest -> (
        match hd with
        | 0 -> false
        | hd ->
            let hds = compare hd 0 in
            if hds != prev_sign then false else aux rest hds)
  in
  aux (List.tl diffs) (compare (List.hd diffs) 0)

let is_safe levels =
  let diffs = get_diffs levels in

  List.for_all
    (fun d ->
      let d = abs d in
      d > 0 && d <= 3)
    diffs
  && same_sign diffs

let check_is_safe_with_removals list =
  let is_safes =
    List.mapi
      (fun n _ ->
        let filtered = List.filteri (fun i _ -> i != n) list in
        is_safe filtered)
      list
  in
  any (fun x -> x) is_safes

let is_safe2 levels =
  match is_safe levels with
  | false -> check_is_safe_with_removals levels
  | true -> true

let part1 levels =
  List.fold_left
    (fun num_safes levels ->
      if is_safe levels then num_safes + 1 else num_safes)
    0 levels

let part2 levels =
  List.fold_left
    (fun num_safes levels ->
      if is_safe2 levels then num_safes + 1 else num_safes)
    0 levels

let () =
  let lines = lines_of_file "bin/inputs/day02.txt" in
  let levels = process_levels lines |> List.of_seq in
  levels |> part1 |> printf "part1: %d\n";
  levels |> part2 |> printf "part2: %d\n"
