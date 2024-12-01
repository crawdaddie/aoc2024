open Util

let sorted_lists lines = 
  let (nums1, nums2) = lines |> Seq.map (fun str ->
    match String.split_on_char ' ' str with
      | num1::_::_::num2::[] -> (int_of_string num1, int_of_string num2)
      | [] -> failwith "no spaces"
      | _ -> failwith "too many spaces"
    )
  |> List.of_seq
  |> List.split
  in
  (List.sort compare nums1 
  ,List.sort compare nums2)
;;

let part1 sorted =
  let nums1, nums2 = sorted
  in List.combine nums1 nums2 |> List.fold_left (fun sum (num1, num2) ->
    let diff = abs (num1 - num2) in
    sum + diff
  ) 0
;;

(*
1 3
2 3
3 3
3 4
3 5
4 9
*)
let rec list_jump list current_num jumps = 
  match list with
  | hd::rest when hd > current_num -> (jumps, list) 
  | hd::rest when hd == current_num -> list_jump rest current_num (jumps + 1)
  | [] -> (jumps, [])
  | _ -> failwith ""
;;


let _part2 sorted =
  let nums1, nums2 = sorted in
  let rec calc_similarities nums1 nums2 sims =
    match (nums1, nums2) with
    | [], _ -> sims
    | hd::_, num2::_ -> (
      Printf.printf "compare %d %d\n" hd num2;
      let (instances_on_left, rest_left) = list_jump nums1 hd 0 in
      match hd < num2 with
      | true -> 
        calc_similarities rest_left nums2 sims
      | false -> 
        let (instances_on_right, rest_right) = list_jump nums2 num2 0 in
        (* Printf.printf "do something with %d (instances on left %d) (instances on right: %d)\n" hd instances_on_left instances_on_right; *)
        calc_similarities rest_left rest_right (sims + (hd * instances_on_left * instances_on_right))
    )
    | _ -> 0
  in 
  calc_similarities nums1 nums2 0
;;

let list_count cond l = 
  let rec aux l c =
    match l with
    | [] -> c
    | hd::rest -> 
      if (cond hd) then aux rest (c + 1) else aux rest c
  in
  aux l 0
;;




let part2 sorted =
  let left, right = sorted in
  let sims, _ = List.fold_left (fun (acc, right) n ->
      let sims = acc + (n * (list_count (Int.equal n) right)) in
      (sims, right)
  ) (0, right) left 
  in
  sims
;;


let () =
  (* let content = input_string "bin/inputs/day01.txt" in *)
  let lines = lines_of_file "bin/inputs/day01.txt" in
  let sorted = sorted_lists lines in
  sorted |> part1 |> Printf.printf "part1 %d\n";
  sorted |> part2 |> Printf.printf "part2 %d\n";
  (* lines |> part2 |> Printf.printf "part2 %d\n"; *)
  ()
