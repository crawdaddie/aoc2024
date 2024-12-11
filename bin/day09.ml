open Util
open Printf

external defrag_disk : string -> int array = "defrag_disk"

let checksum disk =
  Array.fold_left
    (fun (acc, i) v ->
      match v with 0 -> (acc, i + 1) | v -> (acc + (i * (v - 1)), i + 1))
    (0, 0) disk

let part1 content =
  let disk = defrag_disk content in
  let sum, _ = checksum disk in
  sum

let char_to_int ch = Char.code ch - Char.code '0'

let disk_size_in_blocks map =
  String.fold_left (fun acc ch -> acc + char_to_int ch) 0 map

module DiskMap = struct
  type t = int array
  type s = int array

  let make str_map =
    let disk = Array.make (disk_size_in_blocks str_map) 0 in
    let disk, _, _ =
      String.fold_left
        (fun (disk, offset, i) ch ->
          let num_blocks = char_to_int ch in
          let () =
            match i mod 2 with
            | 0 -> Array.fill disk offset num_blocks (1 + (i / 2))
            | _ -> Array.fill disk offset num_blocks 0
          in
          let offset = offset + num_blocks in
          (disk, offset, i + 1))
        (disk, 0, 0) str_map
    in
    let map =
      Array.init (String.length str_map) (fun i -> char_to_int str_map.[i])
    in
    (disk, map)
end

let part2 content =
  let disk, map = DiskMap.make content in
  let hd, tail =
    ((map.(0), 1), (Array.length disk - 1, Array.length map - 1))
  in
  let rec scan hd tail =
    let (l, _), (r, _) = hd, tail in 
    if (l >= r) then disk else (
    )

  1

let () =
  (* let content = input_string "bin/inputs/day09.txt" |> String.trim in *)
  let content = "2333133121414131402" in

  content |> part1 |> printf "part1: %d\n";
  content |> part2 |> printf "part2: %d"
