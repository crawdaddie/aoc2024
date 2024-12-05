open Util
open Printf

let process_order_line x =
  match String.split_on_char '|' x with
  | [ x; y ] -> (int_of_string x, int_of_string y)
  | _ -> failwith "poorly formatted ordering section"

let process_update_line x =
  x |> String.split_on_char ',' |> List.map int_of_string

let sections content =
  let lines = String.split_on_char '\n' content in
  let rec split_sections lines ordering updates after =
    match lines with
    | [] -> (List.rev ordering, List.rev updates)
    | "" :: rest -> split_sections rest ordering updates true
    | str :: rest when after ->
        split_sections rest ordering (process_update_line str :: updates) true
    | str :: rest when not after ->
        split_sections rest (process_order_line str :: ordering) updates false
    | _ -> failwith ""
  in
  split_sections lines [] [] false

let rec list_contains l v =
  match l with
  | [] -> false
  | hd :: rest -> if v = hd then true else list_contains rest v

let rec list_is_contained_in_list l1 l2 =
  match l1 with
  | [] -> true
  | hd :: rest ->
      if not (list_contains l2 hd) then false
      else list_is_contained_in_list rest l2

let check_valid lookup update =
  let rec aux l =
    match l with
    | [] -> true
    | hd :: rest ->
        if not (list_is_contained_in_list rest lookup.(hd)) then false
        else aux rest
  in
  aux update

let part1 lookup updates =
  let _check_valid update =
    let is_valid = check_valid lookup update in
    if is_valid then
      let len = List.length update in
      List.nth update (len / 2)
    else 0
  in

  List.fold_left (fun acc up -> acc + _check_valid up) 0 updates

let order_updates lookup update =
  List.sort
    (fun a b ->
      let after_a = lookup.(a) in
      let after_b = lookup.(b) in
      if list_contains after_a b then -1
      else if list_contains after_b a then 1
      else 0)
    update

let part2 lookup updates =
  let _check_valid update =
    let len = List.length update in
    let is_valid = check_valid lookup update in
    if not is_valid then List.nth (order_updates lookup update) (len / 2) else 0
  in

  List.fold_left (fun acc up -> acc + _check_valid up) 0 updates

let () =
  let content = input_string "bin/inputs/day05.txt" in
  let ordering, updates = sections content in

  let ordering_lookup =
    List.fold_left
      (fun arr (x, y) ->
        let v = arr.(x) in
        Array.set arr x (match v with [ 0 ] -> [ y ] | v -> y :: v);
        arr)
      (Array.init 100 (fun _ -> [ 0 ]))
      ordering
  in
  part1 ordering_lookup updates |> printf "part1: %d\n";
  part2 ordering_lookup updates |> printf "part2: %d\n"
