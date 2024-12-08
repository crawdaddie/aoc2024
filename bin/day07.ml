open Util
open Printf

let get_inputs content =
  let lines = content |> String.split_on_char '\n' in

  lines
  |> List.filter_map (fun str ->
         if str = "" then None
         else
           match String.split_on_char ':' str with
           | [ test_val; operands ] ->
               Some
                 ( int_of_string test_val,
                   String.split_on_char ' ' operands
                   |> List.filter (fun x -> x <> "")
                   |> List.map (fun op -> int_of_string op) )
           | _ -> failwith "poorly formatted input")

let test_operands target ops =
  let rec test ops v =
    if v > target then false
    else
      match ops with
      | [] -> v = target
      | hd :: rest -> test rest (v * hd) || test rest (v + hd)
  in
  test (List.tl ops) (List.hd ops)

let test_operands_extended target ops =
  let rec test ops v =
    if v > target then false
    else
      match ops with
      | [] -> v = target
      | hd :: rest ->
          test rest (v * hd)
          || test rest (v + hd)
          || test rest (concat_numbers v hd)
  in
  test (List.tl ops) (List.hd ops)

let print_input (target, ops) =
  printf "target: %d ops: %s\n" target
  @@ String.concat ", " (List.map string_of_int ops)

let test_input (target, ops) =
  if test_operands target ops then (
    printf "can make ";
    print_input (target, ops))
  else (
    printf "cannot make ";
    print_input (target, ops))
(*
let () =
  printf "tests:\n";
  test_input (190, [ 10; 19 ]);
  test_input (191, [ 10; 19 ]);
  test_input (3267, [ 81; 40; 27 ])
*)

let part1 inputs =
  List.fold_left
    (fun acc (test, operands) ->
      if test_operands test operands then acc + test else acc)
    0 inputs

let part2 inputs =
  List.fold_left
    (fun acc (test, operands) ->
      if test_operands_extended test operands then acc + test else acc)
    0 inputs

let () =
  let content = input_string "bin/inputs/day07.txt" in
  let inputs = content |> get_inputs in
  inputs |> part1 |> printf "part1: %d\n";
  inputs |> part2 |> printf "part2: %d\n"
