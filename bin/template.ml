open Util
open Printf

let part1 content =
  printf "part1\n";
  printf "%s" content;
  1

let part2 content =
  printf "part2\n";
  printf "%s" content;
  1

let () =
  let content = input_string "bin/inputs/${DAY}.txt" in
  content |> part1 |> printf "part1: %d\n";
  content |> part2 |> printf "part2: %d\n"
