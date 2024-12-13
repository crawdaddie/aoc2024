open Util
open Printf

type stone = { int : int; str : string }

let process_stones l =
  List.fold_left
    (fun (l, sum) x ->
      match x with
      | x when x.int = 0 -> ({ int = 1; str = "1" } :: l, sum + 1)
      | x when String.length x.str mod 2 = 0 ->
          let len = String.length x.str in
          let mid = len / 2 in
          let left, right =
            (String.sub x.str 0 mid, String.sub x.str mid mid)
          in
          ( { int = int_of_string left; str = left }
            :: {
                 int = int_of_string right;
                 str = string_of_int (int_of_string right);
               }
            :: l,
            sum + 2 )
      | x ->
          let v = x.int * 2024 in
          ({ int = v; str = string_of_int v } :: l, sum + 1))
    ([], 0) l

let print_list l =
  List.iter (fun x -> printf "%d " x.int) l;
  printf "\n";
  ()

let part1 l =
  let rec loop l num i max =
    match i with
    | i when i = max -> num
    | _ ->
        let l, s = process_stones l in
        loop l s (i + 1) max
  in
  loop l 0 0 25

module IntPairHash = struct
  type t = int * int

  let equal (x1, y1) (x2, y2) = x1 = x2 && y1 = y2
  let hash (x, y) = Hashtbl.hash (x, y)
end

module PairMap = Hashtbl.Make (IntPairHash)

let split_num n =
  let str = string_of_int n in
  let len = String.length str in
  let mid = len / 2 in
  let left, right = (String.sub str 0 mid, String.sub str mid mid) in
  [ int_of_string left; int_of_string right ]

let get_next_stones n =
  if n = 0 then [ 1 ]
  else if String.length (string_of_int n) mod 2 = 0 then split_num n
  else [ n * 2024 ]

let part2 l =
  let map = PairMap.create 200 in
  let rec num_from_stones stone iter =
    match iter with
    | 0 -> 1
    | _ -> (
        match PairMap.find_opt map (stone, iter) with
        | Some value -> value
        | None ->
            let res =
              get_next_stones stone
              |> List.fold_left
                   (fun acc st -> acc + num_from_stones st (iter - 1))
                   0
            in
            PairMap.add map (stone, iter) res;
            res)
  in
  List.fold_left (fun acc stone -> acc + num_from_stones stone 75) 0 l

let () =
  let content = input_string "bin/inputs/day11.txt" in
  (* let content = "125 17" in *)
  let l =
    content |> String.split_on_char ' '
    |> List.map (fun x ->
           let t = String.trim x in
           { int = int_of_string t; str = t })
  in

  l |> part1 |> printf "part1: %d\n";

  content |> String.split_on_char ' '
  |> List.map (fun x ->
         let t = String.trim x in
         int_of_string t)
  |> part2 |> printf "part2: %d"
