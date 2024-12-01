open Util

let part1 content =
  Printf.printf "part1\n";
  Printf.printf "%s" content
;;

let part2 content =
  Printf.printf "part2\n";
  Printf.printf "%s" content
;;

let () =
  let content = input_string "bin/inputs/${DAY}.txt" in
  content |> part1;
  content |> part2
