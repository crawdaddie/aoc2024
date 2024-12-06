open Util
open Printf

module BoundedSet = struct
  (* Creates an empty set *)
  let empty () = Array.make 100 false

  (* Check if a value is in bounds *)
  let is_valid n = n >= 0 && n < 100

  (* Add an element to the set *)
  let add set n =
    if not (is_valid n) then
      raise (Invalid_argument "Value must be between 0 and 100")
    else (
      set.(n) <- true;
      set (* Return the modified set *))

  (* Creates a new set with i *)
  let from i =
    let s = empty () in
    add s i

  (* Check if an element is in the set *)
  let mem set n =
    if not (is_valid n) then
      raise (Invalid_argument "Value must be between 0 and 100")
    else set.(n)

  (* Check if a list is contained in the set *)
  let rec contains_list set l =
    match l with
    | [] -> true
    | hd :: rest -> if not (mem set hd) then false else contains_list set rest
end

let process_order_line x =
  match String.split_on_char '|' x with
  | [ x; y ] -> (int_of_string x, int_of_string y)
  | _ -> failwith "poorly formatted ordering section"

let process_update_line x =
  x |> String.split_on_char ',' |> List.map int_of_string

let check_valid lookup update =
  let rec aux l =
    match l with
    | [] -> true
    | hd :: rest -> (
        match lookup.(hd) with
        | None -> failwith "no instructions for update int"
        | Some set ->
            if not (BoundedSet.contains_list set rest) then false else aux rest)
  in
  aux update

let order_updates lookup update =
  List.sort
    (fun a b ->
      let after_a = lookup.(a) in
      let after_b = lookup.(b) in
      match (after_a, after_b) with
      | Some after_a, Some after_b ->
          if BoundedSet.mem after_a b then -1
          else if BoundedSet.mem after_b a then 1
          else 0
      | _ -> failwith "no instructions for update int")
    update

let () =
  let content = input_string "bin/inputs/day05.txt" in
  let ordering, updates = split_on_empty_line content in
  let ordering, updates =
    (List.map process_order_line ordering, List.map process_update_line updates)
  in

  let lookups =
    List.fold_left
      (fun arr (x, y) ->
        let v = arr.(x) in
        arr.(x) <-
          (match v with
          | None -> Some (BoundedSet.from y)
          | Some s -> Some (BoundedSet.add s y));
        arr)
      (Array.make 100 None) ordering
  in

  let valid, invalid = List.partition (check_valid lookups) updates in

  let get_mid up =
    let len = List.length up in
    List.nth up (len / 2)
  in

  List.fold_left (fun acc up -> acc + get_mid up) 0 valid
  |> printf "part1: %d\n";

  List.fold_left
    (fun acc up -> acc + (up |> order_updates lookups |> get_mid))
    0 invalid
  |> printf "part2: %d\n"
