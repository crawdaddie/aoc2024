open Util
open Printf

let char_to_int ch = int_of_char ch - int_of_char '0'

let part1 content =
  let len = String.length content in
  let num_blocks =
    String.fold_left (fun acc ch -> acc + char_to_int ch) 0 content
  in
  let disk = Array.init num_blocks (fun i -> -1) in
  let disk =
    String.fold_left
      (fun (offset, disk) ch ->
        let num_blocks = char_to_int ch in
        let offset = offset + num_blocks in
        (offset, disk))
      (0, disk) content
  in
  printf "num blocks: %d\n" num_blocks;
  1

let part2 content = 1

let () =
  let content = input_string "bin/inputs/day09.txt" in
  (* let content = "2333133121414131402" in *)
  (* 00...111...2...333.44.5555.6666.777.888899 *)
  content |> part1 |> printf "part1: %d\n";
  content |> part2 |> printf "part2: %d\n"
