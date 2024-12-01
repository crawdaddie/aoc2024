open Printf
open Util

let sorted_lists lines =
  let nums1, nums2 =
    lines
    |> Seq.map (fun str ->
           match String.split_on_char ' ' str with
           | [ num1; _; _; num2 ] -> (int_of_string num1, int_of_string num2)
           | [] -> failwith "no spaces"
           | _ -> failwith "too many spaces")
    |> List.of_seq |> List.split
  in
  (List.sort compare nums1, List.sort compare nums2)

let part1 (left, right) =
  List.combine left right
  |> List.fold_left
       (fun sum (num1, num2) ->
         let diff = abs (num1 - num2) in
         sum + diff)
       0

(**
take a sorted list l and a number n, count the number of occurences of n in l from the beginning
return the count, and the rest of the list after the last occurence of n
*)
let count_and_jump l n =
  let rec aux l c =
    match l with
    | [] -> (l, c)
    | hd :: rest -> (
        match (hd, n) with
        | hd, n when hd > n -> (l, c)
        | hd, n when hd = n -> aux rest (c + 1)
        | hd, n when hd < n -> aux rest c
        | _ -> aux rest c)
  in
  aux l 0

let part2 (left, right) =
  let rec aux left right sim_score =
    match (left, right) with
    | [], _ -> sim_score
    | _, [] -> sim_score
    | lhd :: _, rhd :: _ ->
        let left, lc = count_and_jump left lhd in
        if lhd < rhd then aux left right sim_score
        else
          let right, rc = count_and_jump right lhd in
          aux left right sim_score + (lhd * lc * rc)
  in
  aux left right 0

let () =
  let lines = lines_of_file "bin/inputs/day01.txt" in
  let sorted = sorted_lists lines in
  sorted |> part1 |> printf "part1 %d\n";
  sorted |> part2 |> printf "part2 %d\n";
  ()
